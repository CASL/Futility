#!/usr/bin/python

import __future__
import argparse
import glob
import sys
import os
import re
import itertools as it
import pandas as pd
import warnings
import math
#from IPython.display import HTML

# Define comand line arguments
parser = argparse.ArgumentParser(description='Searches recursively through system path to find tagged' +
                                 ' requirements in files, extracts them, and compiles them into a table in a latex file.')
parser.add_argument('--path', type=str,
                    help='The path to search recursively', default='.')
parser.add_argument('--ext', action='append', type=str,
                    help='The file extensions to check in for requirements', default=[])
parser.add_argument('--output', type=str,
                    help='Base name of the output file', default='requirements')
parser.add_argument('--skip-no-require', dest='skip_no_require', action='store_true',
                    help='If added, files with no requirements present will not be added to the latex file')
parser.add_argument('--cdash-test-name-file',dest='cdash_test_names',type=str,
                    help="""
                         Path and file name of text file generated at TRIBITS configure time
                         that contains list of CMake generated test names and test inputs
                         """
                   )
args = parser.parse_args()

# Requirement ID Counter
reqID = it.count()

################################################################################


class Requirement:
    """ Define a requirement

    This class takes a requirement block as input, which is a block of text (formatted as a list of
    strings) that identifies a single requirement.  It shall be formatted as:

    !> @beginreq
    !> - Requirement description which
    !> may span mutliple lines
    !> - ticket 0000
    !> @endreq

    There may be any number of spaces or characters prior to the comment tag "!>".
    There may be any number of spaces before or after the hyphens.
    The hyphen identifies a new requirement attribute.
    Currently a requirement may have a description (no keyword) or a ticket number.
    Any lines without a hyphen are interpretted as continuations of the previous line.

    Args:
       ID (int): Numeric identifier for this requirement
       testName (str): The name of the test that shows up in the test report. This is 
                       nominally the test name created by CMake that appears on CDash.
       testFile (str): The name of the file where this requirement was found
       rawReqBlock (list): See above documentation for a detailed description.  Passing None means this
          is not a requirement.
    """

    ### Attributes ###
    # Private regex
    _re_int = re.compile(r'[0-9]+')     # find integer ticket number
    # find the first line of a req. descr.
    _re_desc = re.compile('.*!>\s*-\s*')
    _re_contl = re.compile('.*!>\s*')     # find continuation lines
    _re_ticket = re.compile('.*!>\s*-\s*ticket\s*',
                            flags=re.IGNORECASE)  # find ticket entry

    descr = None
    tix = None
    tname = None

    def __init__(self, ID, testName, testFile, rawReqBlock):
        self.ID = ID
        if "/" ==  args.path[-1]:
            self.tfile = testFile[len(args.path):]
        else:
            self.tfile = testFile[len(args.path)+1:]
        self.tname = testName
        if self.tname is None:
            self.tname = ""
        if rawReqBlock:
            match = self._re_desc.match(rawReqBlock[1])
            if not match:
                # If a @beginreq block is present, the description portion of the
                # requirement needs to be present and formatted correctly
                raise RuntimeError(
                    'Requirement not formatted correctly in file: '+testFile+' block: '+rawReqBlock[1])
            # Strip out any characters prior to the beginning of the text description of the requirement
            self.descr = rawReqBlock[1][match.end():]

            # Read through the rest of the block and append any continuation lines to the description
            # 0 was the @beginreq tag, 1 is the requirements description
            lineNum = 2
            for line in rawReqBlock[lineNum:-1]:
                # If a new block parameter is found, stop looking for continuation lines
                if self._re_ticket.match(line):
                    break
                lineNum = lineNum+1
                try:
                    self.descr = self.descr+line.split('!>')[1].rstrip()
                except:
                    raise RuntimeError(
                        "Error process file: "+testFile+" line: "+line)
            # Check for a ticket number in the block and save
            # Defaults to None if not present
            if lineNum < len(rawReqBlock):
                match = self._re_ticket.match(rawReqBlock[lineNum])
                if match:
                    tix = self._re_ticket.split(
                        rawReqBlock[lineNum])[1].rstrip()
                    if self._re_int.match(tix):
                        self.tix = ''
                        for t in self._re_int.findall(tix):
                            self.tix += 'https://vminfo.casl.gov/trac/casl_phi_kanban/ticket/'+t+'\n'
                        self.tix = self.tix.rstrip()
                    else:
                        self.tix = tix

    def __str__(self):
        return '          Requirement ID: ' + str(self.ID) + '\n'\
            + 'Requirement  Description: ' + str(self.descr)+'\n'\
            + '               Test Name: ' + str(self.tname)+'\n'\
            + '               Test File: ' + str(self.tfile)+'\n'\
            + '  Additional Information: ' + str(self.tix) + '\n'

    def to_dict(self):
        return {
            'Requirement ID': self.ID,
            'Requirement Description': self.descr,
            'Test Name': self.tname,
            'Test File': self.tfile,
            'Additional Information': self.tix,
        }

################################################################################


class File_RequirementParser:
    """ Extracts requirements from input file

    The input file can be any text file.
    The class will read through the entire file and find all requirement blocks (see Requirement for a description).
    A Requirement object will be created for each requirement block that was found.
    If no requirement blocks are found in the file, one empty Requirement object will be created for that file.

    Args:
       testFile (str): Name of the file to search (full file path)
    """
    _re_begin = re.compile(r'!> @beginreq\n', re.IGNORECASE)
    _re_end = re.compile(r'!> @endreq\n', re.IGNORECASE)

    def __init__(self, testFile):
        self.allReqs = []

        if not os.path.isfile(testFile):
            return

        cdash_test_name = None
        if args.cdash_test_names:
            if os.path.isfile(args.cdash_test_names):
                f_tnames = open(args.cdash_test_names, 'r')
                fline = f_tnames.readline()
                while fline:
                    cdash_test_input = fline.split(" ")[1]
                    cdash_test_input = cdash_test_input.rstrip("\n")
                    if cdash_test_input in testFile:
                       cdash_test_name = fline.split(" ")[0]
                       break
                    fline = f_tnames.readline()

        if sys.version_info[0] < 3:
            fobject = open(testFile, 'r')
        else:
            fobject = open(testFile, 'r', errors='ignore')
        fline = fobject.readline()
        while fline:
            if self._re_begin.search(fline):
                reqBlock = []
                reqBlock.append(fline)
                endmatch = False 
                while fline:
                    fline = fobject.readline()
                    reqBlock.append(fline)
                    if self._re_end.search(fline):
                        newID = next(reqID)+1
                        self.allReqs.append(Requirement(
                            newID, cdash_test_name, testFile, reqBlock))
                        endmatch = True
                        break
                if not endmatch:
                    raise RuntimeError(
                        '@beginreq was found but @endreq was not found in file: '+ testFile)
            fline = fobject.readline()

        if not self.allReqs:
            self.allReqs.append(Requirement(None, cdash_test_name, testFile, None))

    def __nonzero__(self):
        return bool(self.allReqs)

    def __str__(self):
        s = ''
        for r in self.allReqs:
            s += str(r)
        return s

    def __getitem__(self, index):
        return self.allReqs[index]

    def __len__(self):
        return len(self.allReqs)

################################################################################
# Routines to assist in formatting HTML table


def hover(hover_color='yellow'):
    return dict(selector='tr:hover',
                props=[('background-color', '%s' % hover_color)])


def color_odd_rows(odd_color='#E9EDF4'):
    return dict(selector='tr:nth-child(odd)',
                props=[('background', '%s' % odd_color)]),


def color_even_rows(even_color='#D0D8E8'):
    return dict(selector='tr:nth-child(even)',
                props=[('background', '%s' % even_color)])


def table_header_styles():
    return [
        ('font-size', '14pt'),
        ('text-align', 'center'),
        ('font-weight', 'bold'),
        ('color', 'white'),
        ('background-color', '#4F81BD'),
    ]


def table_cell_styles():
    return [
        ('font-size', '12pt'),
        ('padding', '5px'),
    ]


def html_table_style():
    return [
        color_odd_rows(),
        color_even_rows(),
        hover(),
        dict(selector='th', props=table_header_styles()),
        dict(selector='td', props=table_cell_styles()),
    ]


def convert_url_to_html_hyperlink(cell_urls):
    if cell_urls:
        s = ''
        for url in cell_urls.split('\n'):
            if url.startswith('https://') or url.startswith('http://'):
                s += '<a href='+url+'>&#35;'+url.split('/')[-1]+'</a><br/>'
            else:
                s += url  # Return non url content unmodified
        return s
    else:
        return cell_urls

################################################################################
# Primary routines


def ConvertToHTML(df):
    """ Convert a Pandas DataFrame to a pretty HTML table

      The pandas data frame should have a an entry "Additional Information".

      Args:
         df (pandas.DataFrame): A pandas data frame

      Returns:
        str: HTML text for table
    """
    df['Additional Information'] = df['Additional Information'].apply(
        convert_url_to_html_hyperlink)
    html_table = (df.style
                  .highlight_null(null_color='red')
                  .set_table_styles(html_table_style())
                  .render())
    return html_table

def ConvertToLatex(df):
    """ Convert a Pandas DataFrame to a pretty LaTeX table

      The pandas data frame should have an entry "Additional Information".

      Args:
         df (pandas.DataFrame): A pandas data frame

      Returns:
        str: LaTeX text for table
    """
    #df['Additional Information'] = df['Additional Information'].apply(
        #convert_url_to_html_hyperlink)
    #latex_table = (df.style
                  #.highlight_null(null_color='red')
                  #.set_table_styles(html_table_style())
                  #.render())
    #latex_table = df.to_latex(multicolumn=False,multirow=True,column_format='ll')

    #Header
    ws = '                   '

    latex_table = '\\begin{longtable}[!ht]{|p{1.4cm}|p{14cm}|}\n' + \
                  '\\caption{Requirements}\\label{tab:req}\\\\\n' + \
                  '\n\\hline\n' + \
                  '\\multirow{4}{*}{\\textbf{Req. ID}} & \\cellcolor{caslheader}\\textcolor{white}{\\textit{Requirement Description}} \\\\\\cline{2-2}\n' + \
                  ws + '& \cellcolor{caslcolor1} \\textit{Test Name} \\\\\\cline{2-2}\n' + \
                  ws + '& \cellcolor{caslcolor2} \\textit{Test Input} \\\\\\cline{2-2}\n' + \
                  ws + '& \cellcolor{caslcolor1} \\textit{Additional Info} \\\\\\hline\n' + \
                  '\\endfirsthead\n\n' + \
                  '\\hline\n' + \
                  '\\multirow{4}{*}{\\textbf{Req. ID}} & \\cellcolor{caslheader}\\textcolor{white}{\\textit{Requirement Description}} \\\\\\cline{2-2}\n' + \
                  ws + '& \cellcolor{caslcolor1} \\textit{Test Name} \\\\\\cline{2-2}\n' + \
                  ws + '& \cellcolor{caslcolor2} \\textit{Test Input} \\\\\\cline{2-2}\n' + \
                  ws + '& \cellcolor{caslcolor1} \\textit{Additional Info} \\\\\\hline\n' + \
                  '\\endhead\n' + \
                  '\\hline\n' + \
                  '\\endfoot\n' + \
                  '\\hline\n' + \
                  '\\endlastfoot\n\n'

    mr = '\multirow{4}{*}{'
    pathpre = os.getcwd()
    pathpre = pathpre.replace('_',r'\_')

    #Data
    i=0
    for index, row in df.iterrows():
      #Trim the last entry, which looks like a return.  Escape %, &, and escape _.
      reqdesc = str(row['Requirement Description'])[:-1].replace('%',r'\%')
      reqdesc = reqdesc.replace('_',r'\_')
      reqdesc = reqdesc.replace('&',r'\&')

      #Escape % and escape _.
      testfile = row['Test File'].replace('%',r'\%')
      testfile = testfile.replace('_',r'\_')
      if (len(testfile) > 80):
        insert_space = testfile.rfind('/',0,80)
        testfile = testfile[:insert_space+1]+"\\newline "+testfile[insert_space+1:]
      #cdash
      cdashname = row['Test Name'].replace('_',r'\_')
      cdashname = cdashname.replace('%',r'\%')
      #Escape % and escape _.
      addinfo = str(row['Additional Information']).replace('_',r'\_')
      addinfo = addinfo.rstrip()
      if (len(addinfo) > 0) and ('/' in addinfo):
        addinfo = "\href{" + addinfo + "}{\# " + addinfo[addinfo.rfind('/')+1:] + "}"
      rowid = ''
      if (not math.isnan(row['Requirement ID'])):
        rowid = str(int(row['Requirement ID']))
      latex_table += mr + rowid + '} & \\cellcolor{caslheader}\\textcolor{white}{' + reqdesc + r"} \\\cline{2-2}" + '\n'
      latex_table += ws + '& \\cellcolor{caslcolor1}' + cdashname + r" \\\cline{2-2}" +'\n'
      latex_table += ws + '& \\cellcolor{caslcolor2}' + testfile + r" \\\cline{2-2}" +'\n'
      latex_table += ws + '& \\cellcolor{caslcolor1}' + addinfo + r" \\\hline" + '\n'
      i += 1
      if (i % 6 == 5):
        latex_table += '\\newpage'

    #Footer
    latex_table = latex_table + '\\end{longtable}\n'
    return latex_table

def GenerateInputList(path='', ext=[]):
    """ Collect a list of all the files to be parsed given a path and list of extensions

    Args:
       path (str): Path to start searching from.  Will walk all subdirectories starting from this path.
       ext (list): List of file extensions to include in the list.  By default includes all files.

    Returns:
       list: List of files to be parsed

    """
    searchPath = os.path.dirname(os.path.realpath(__file__))
    searchPath = searchPath + path
    searchPath = path
    inputs = []
    for root, dirs, files in os.walk(searchPath):
        for file in files:
            if len(ext) == 0:
                inputs.extend(glob.glob(os.path.join(root, file)))
            else:
                for e in ext:
                    if file.endswith(e):
                        inputs.append(os.path.join(root, file))
    return inputs


def CreateRequirementsDataFrame():
    """ Generate a Pandas DataFrame of requirements

    Returns:
      pandas.DataFrame: all requirements
    """
    allReqs = []
    for f in GenerateInputList(args.path, args.ext):
        for r in File_RequirementParser(f):
            if args.skip_no_require and not r.descr:
                # The user wants to skip files with no requirements
                addFile = False
            else:
                addFile = True
            if addFile:
                allReqs.append(r.to_dict())

    # Convert list of requirements to Pandas DataFrame
    table_headers = ['Requirement ID', 'Requirement Description', 'Test Name',
                     'Test File', 'Additional Information']
    df = pd.DataFrame(data=allReqs, columns=table_headers)
    df = df.sort_values('Requirement ID')
    return df


################################################################################
# Main
if __name__ == "__main__":
    reqDF = CreateRequirementsDataFrame()
    #reqHTML = ConvertToHTML(reqDF)
    reqLatex = ConvertToLatex(reqDF)

    # Write to HTML file
    #f = open(args.output+'.html', 'w')
    #f.write(reqHTML)
    #f.close()
    f = open(args.output+'.tex', 'w')
    f.write(reqLatex)
    f.close()

