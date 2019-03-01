#!/usr/bin/python

#TODO: Figure out how to force column widths
#TODO: Potentially add borders
#TODO: Check against CMakeLists
#TODO: improve parsing of text file

#NOTE: Requirements are indexed as they are found.  This means requirement numbers can change as new
#      tests are added in the future.  

import __future__
import argparse
import glob
import sys
import os
import re
import itertools as it
import pandas as pd
import warnings
from IPython.display import HTML

parser=argparse.ArgumentParser(description="Searches recursively through system path to find tagged"+
      " requirements in files, extracts them, and compiles them into a table in an html file.")
parser.add_argument("--path",type=str,help="The path to search recursively",default=".")
parser.add_argument("--ext",action='append',type=str,help="The file extensions to check in for requirements",default=[])
parser.add_argument("--output",type=str,help="Name of the output file",default="requirements.html")
parser.add_argument("--skip-no-require", dest="skip_no_require",action="store_true",help="If added, files with no requirements present will not be added to the HTML file")
args = parser.parse_args()

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
     testFile (str): The name of the file where this requirement was found
     rawReqBlock (list): See above documentation for a detailed description.  Passing None means this
        is not a requirement.
  """
  # Used to find integer ticket number
  _re_int = re.compile(r'[0-9]+')
  # Used to find the first line of a requirement description
  re_desc = re.compile('.*!>\s*-\s*')
  # Used to find continuation lines in the requirement block
  re_continue_line = re.compile('.*!>\s*')
  # Used to find a ticket attribute of a requirement
  re_ticket = re.compile('.*!>\s*-\s*ticket\s*', flags=re.IGNORECASE)
  descr = None
  tix = None

  def __init__(self,ID,testFile,rawReqBlock):
      self.ID    = ID
      self.tfile = testFile
      if rawReqBlock:
          match = self.re_desc.match(rawReqBlock[1])
          if not match:
              # If a @beginreq block is present, the description portion of the requirement needs to be present and formatted correctly
              warnings.warn("Requirement not formatted correctly in file: "+testFile)
          # Strip out any characters prior to the beginning of the text description ofthe requirement
          self.descr = rawReqBlock[1][match.end():]
          # Read through the rest of the block and append any continuation lines to the description
          # 0 was the @beginreq tag, 1 is the requirements description
          lineNum = 2
          for line in rawReqBlock[lineNum:-1]:
              # If a new block parameter is found, stop looking for continuation lines
              if self.re_ticket.match(line): break
              lineNum = lineNum+1
              self.descr = self.descr+line.split('!>')[1].rstrip()
          # Check for a ticket number in the block and save
          # Defaults to None if not present
          if lineNum<len(rawReqBlock):
              match = self.re_ticket.match(rawReqBlock[lineNum], re.IGNORECASE)
              if match:
                  tix = self.re_ticket.split(rawReqBlock[lineNum])[1].rstrip()
                  if self._re_int.match(tix):
                      self.tix   = ""
                      for t in self._re_int.findall(tix):
                          self.tix += 'https://vminfo.casl.gov/trac/casl_phi_kanban/ticket/'+t+'\n'
                      self.tix = self.tix.rstrip()
                  else:
                      warnings.warn("Not a valid ticket number in requirement block in file: "+testFile)

  def __str__(self):
      return "Requirement ID: " + str(self.ID)   +'\n'\
           + "   Description: " + str(self.descr)+'\n'\
           + "        Ticket: " + str(self.tix)  +'\n'\
           + "          File: " + str(self.tfile)+'\n'

  def to_dict(self):
      return {
               "Requirement ID": self.ID,
                  "Description": self.descr,
                      "Tickets": self.tix,
                         "File": self.tfile,
             }

################################################################################
class MPACT_RequirementParser:
    """ Extracts requirements from input file

    The input file can be any text file.
    The class will read through the entire file and find all requirement blocks (see Requirement for a description).
    A Requirement object will be created for each requirement block that was found.
    If no requirement blocks are found in the file, one empty Requirement object will be created for that file.

    Args:
       testFile (str): Name of the file to search (full file path)
    """
    _re_begin = re.compile(r'!> @beginreq\n',re.IGNORECASE)
    _re_end   = re.compile(r'!> @endreq\n',re.IGNORECASE)

    def __init__(self, testFile):
        self.allReqs = []

        if not os.path.isfile(testFile):
            return

        if sys.version_info[0] < 3:
            fobject = open(testFile,"r")
        else:
            fobject = open(testFile,"r",errors='ignore')
        fline = fobject.readline()
        while fline:
            if self._re_begin.search(fline):
                reqBlock = []
                reqBlock.append(fline)
                while fline:
                    fline = fobject.readline()
                    reqBlock.append(fline)
                    if self._re_end.search(fline):
                        newID = next(reqID)+1
                        self.allReqs.append(Requirement(newID,testFile,reqBlock))
                        break
            fline = fobject.readline()

        if not self.allReqs:
          self.allReqs.append(Requirement(None,testFile,None))

    def __nonzero__(self):
      return bool(self.allReqs)

    def __str__(self):
        s=""
        for r in self.allReqs:
            s += str(r) 
        return s

    def __getitem__(self,index):
        return self.allReqs[index]

    def __len__(self):
        return len(self.allReqs)

################################################################################
#Routines to assist in formatting HTML table
def hover(hover_color="yellow"):
    return dict(selector="tr:hover",
                props=[("background-color", "%s" % hover_color)])

def color_odd_rows(odd_color="#E9EDF4"):
    return dict(selector="tr:nth-child(odd)",
                props=[("background", "%s" % odd_color)]),

def color_even_rows(even_color="#D0D8E8"):
    return dict(selector="tr:nth-child(even)",
                props=[("background", "%s" % even_color)])

def convert_url_to_html_hyperlink(cell_urls):
  if cell_urls:
    s=""
    for url in cell_urls.split("\n"):
      s += "<a href="+url+">&#35;"+url.split("/")[-1]+"</a><br/>"
    return s
  else:
    return None

def GenerateInputList(path="/../MPACT_exe/tests", ext=[]):
    """ Collect a list of all the files to be parsed

    Args:
       path (str): Path to start searching from.  Will walk all subdirectories starting from this path.
       ext (list): List of file extensions to include in the list.  By default includes all files.

    """
    searchPath = os.path.dirname(os.path.realpath(__file__))
    searchPath = searchPath + path
    searchPath = path
    inputs = []
    for root, dirs, files in os.walk(searchPath):
        for file in files:
            if len(ext)==0:
                inputs.extend(glob.glob(os.path.join(root,file)))
            else:
                for e in ext:
                    if file.endswith(e):
                        inputs.append(os.path.join(root,file))
    return inputs

#Process test inputs
allReqs = [];
for f in GenerateInputList(args.path, args.ext):
   for r in MPACT_RequirementParser(f):
       if args.skip_no_require and not r.descr:
           # The user wants to skip files with no requirements
           addFile = False
       else:
           addFile = True
       if addFile:
           allReqs.append(r.to_dict())

#Convert list of requirements to Pandas DataFrame
table_headers = ['Requirement ID','Description','File','Tickets']
df = pd.DataFrame(data=allReqs,columns=table_headers)
df = df.sort_values('Requirement ID')

#Convert URL's to HTML links
df["Tickets"] = df["Tickets"].apply(convert_url_to_html_hyperlink)

# Set style for table header
th_props = [
  ('font-size', '14pt'),
  ('text-align', 'center'),
  ('font-weight', 'bold'),
  ('color', 'white'),
  ('background-color', '#4F81BD'),
  ]

# Set style for cell
td_props = [
  ('font-size', '12pt'),
  ('padding', '5px'),
  ]

# Set table styles
styles = [
  color_odd_rows(),
  color_even_rows(),
  hover(),
  dict(selector="th", props=th_props),
  dict(selector="td", props=td_props),
  ]

# Generate HTML Table as string
html_table = (df.style 
                .highlight_null(null_color='red')
                .set_table_styles(styles) 
                .render())

# Write to file
f = open(args.output,'w')
f.write(html_table)
