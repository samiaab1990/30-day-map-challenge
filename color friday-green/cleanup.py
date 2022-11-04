import PyPDF2
import pandas as pd
import numpy as np
import string

# Create a pdf file object
pdf_file_object = open('SA_lc_Map_41class_aux.pdf', 'rb')
  
# Create a pdf reader object 
pdf_reader = PyPDF2.PdfFileReader(pdf_file_object)
  
# Get number of pages in pdf file
pdf_reader_pages = pdf_reader.numPages
  
# concatenate pdf
full_pdf = ''
for page in range(pdf_reader_pages):
    pdf_page = pdf_reader.getPage(page)
    full_pdf += pdf_page.extractText()

# close pdf file 
pdf_file_object.close()

# subset to everything after class lookup values
split_page = full_pdf.split("Class Lookup Values:")

# further subset to everything before references
data_dictionary = split_page[1].split("References:")[0].split()

# extract value 
value = []
for word in data_dictionary:
    if word.isdigit():
        value.append(word)
value[11] = 11
value = np.delete(value,12)

# extract class name
string = []
string_concat = ''
for word in data_dictionary:
    if word.isdigit() == False:
        string_concat = string_concat + ' ' + word
    else:
        string.append(string_concat.strip())
        string_concat = ''
string.append(string_concat)

# remove spaces, period, and question mark from class name and capitalize first letter
class_name = [string_val for string_val in string if string_val!=""][1:]
class_name = [string_val.replace(".","").replace("?","").strip().capitalize() for string_val in class_name]

# create data frame
data_frame = []
for i in range(len(value)):
    data_point = []
    data_point.append(value[i])
    data_point.append(class_name[i])
    data_frame.append(data_point)

df = pd.DataFrame(data_frame, columns = ['Value','Class Name'])

# save to csv
df.to_csv('data_dictionary_csv.csv', index=False)  
