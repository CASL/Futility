#!/usr/bin/python

#TODO: Figure out how to force column widths
#TODO: Potentially add borders
#TODO: Check against CMakeLists
#TODO: improve parsing of text file

import __future__
import sys
import os
import re
import itertools as it
import pandas as pd
from IPython.display import HTML

reqID = it.count()

################################################################################
class Requirement:
  """Define a requirement"""
  _re_int = re.compile(r'[0-9]+')

  def __init__(self,ID,testFile,rawReqBlock):
      self.ID    = ID
      self.tfile = testFile.split('../')[1].rstrip()
      if rawReqBlock:
          self.descr = rawReqBlock[1].split('!> - ')[1].rstrip()
          tix = re.split('!> - ticket ',rawReqBlock[2],flags=re.IGNORECASE)[1].rstrip()
          if self._re_int.match(tix):
              self.tix   = ""
              for t in self._re_int.findall(tix):
                  self.tix += 'https://vminfo.casl.gov/trac/casl_phi_kanban/ticket/'+t+'\n'
              self.tix = self.tix.rstrip()
          else:
              self.tix = None
      else:
          self.descr = None
          self.tix = None

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
    """Set up regular expressions"""
    _re_begin = re.compile(r'!> @beginreq\n',re.IGNORECASE)
    _re_end   = re.compile(r'!> @endreq\n',re.IGNORECASE)

    def __init__(self, testFile):
        self.allReqs = [];

        if sys.version_info[0] < 3:
            fobject = open(testFile,"r")
        else:
            fobject = open(testFile,"r",errors='ignore')
        fline = fobject.readline()
        print(testFile)
        while fline:
            if self._re_begin.match(fline):
                reqBlock = []
                reqBlock.append(fline)
                while fline:
                    fline = fobject.readline()
                    reqBlock.append(fline)
                    if self._re_end.match(fline):
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

# Collect a list of all the files to be parsed
def GenerateInputList(path="/../MPACT_exe/tests"):
    searchPath = os.path.dirname(os.path.realpath(__file__))
    searchPath = searchPath + path
    inputs = []
    for root, dirs, files in os.walk(searchPath):
        for file in files:
            if file.endswith(".inp"):
                inputs.append(os.path.join(root,file))
    return inputs

#Process test inputs
allReqs = [];
for f in GenerateInputList():
   for r in MPACT_RequirementParser(f):
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
f = open('mpact_requirements.html','w')
f.write(html_table)
f.close()

