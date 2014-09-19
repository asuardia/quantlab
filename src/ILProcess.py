import os, tempfile, json, sys

def transform(data):
    ndict = dict()
    # creates new dictionary with the original tuple converted to json string
    for key,value in data.iteritems():
        nkey = str(key)
        ndict[nkey] = transform(value) if type(dict())==type(value) else value
    return ndict

def process(action, fileIn, fileOut):
    fhdl,pathIn= tempfile.mkstemp()
    il = eval (open(fileIn).read ())
    f = os.fdopen(fhdl, 'w')
    f.write(json.dumps(transform (il)))
    f.close()
    os.system("runhaskell C:\Projects\quantlab\src\Interfaces\\" + action + ".hs " + pathIn + " " + fileOut)
    os.remove(pathIn)

'''  
fhdl,pathIn= tempfile.mkstemp()
il = eval (open(sys.argv[2]).read ())
f = os.fdopen(fhdl, 'w')
f.write(json.dumps(transform (il)))
f.close()
action = sys.argv[1]
fileOut = sys.argv[3]
os.system("C:\\Projects\\quantlab\\src\\Interfaces\\" + action + ".exe " + pathIn + " " + fileOut)
os.remove(pathIn)    
'''
