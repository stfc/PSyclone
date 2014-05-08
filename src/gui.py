#!/usr/bin/env python
from Tkinter import *
import Tkinter as tk
import tkFileDialog
from parse import parse,ParseError
from psyGen import PSy,GenerationError
import os

class callWidget:
    def __init__(self,canvas,name,x,y,width=100,height=30):
        self._canvas=canvas
        self._name=name
        self._x=x
        self._y=y
        self._width=width
        self._height=height
        self._create()
    def _create(self):
        callShape=self._canvas.create_rectangle(self._x,self._y,self._x+self._width,self._y+self._height,outline="red",fill="yellow",activeoutline="blue",width=2)

        self._canvas.create_text(self._x+self._width/2, self._y+self._height/2, text=self._name)
        
class loopWidget:
    def __init__(self,canvas,name,x,y,calls):
        self._canvas=canvas
        self._name=name
        self._x=x
        self._y=y
        self._callWidth=100 # get from calls
        self._callHeight=30 # get from calls
        self._nCalls=1 # get from calls
        self._loopWidth=30
        self._loopHeight=30
        self._create()
    def nCalls(self,ncalls):
        self._nCalls=ncalls
        self._create()
    def position(self,x,y):
        self._x=x
        self._y=y
        self._create()
    def _create(self):
        loopShape = self._canvas.create_polygon(self._x,self._y, self._x+self._loopWidth+self._callWidth,self._y, self._x+self._loopWidth+self._callWidth,self._y+self._loopHeight, self._x+self._loopWidth,self._y+self._loopHeight, self._x+self._loopWidth,self._y+self._loopHeight+self._nCalls*self._callHeight, self._x,self._y+self._loopHeight+self._nCalls*self._callHeight,outline="red",fill="green",width=2,activeoutline="blue",joinstyle=ROUND)
        text = self._canvas.create_text(self._x+(self._loopWidth+self._callWidth)/2, self._y+self._callHeight/2, text=self._name)


class PSyclone(tk.Frame):

    def __init__(self, master=None):
        tk.Frame.__init__(self, master)
        self.grid(row=0,column=0,sticky="nesw")
        #self.rowconfigure(None,0,weight=1)
        #self.columnconfigure(None,0,weight=1)
        self.createWidgets()

    def hello(self):
        print "HELLO"

    def createWidgets(self):

        toolbar=Frame(self)
        toolbar.pack()
        #self.aMenu = tk.Menu(toolbar, tearoff=0)
        #self.aMenu.add_command(label="Undo", command=self.hello)
        #self.aMenu.add_command(label="Redo", command=self.hello)

        self.mb=tk.Menubutton(self, text="File")
        self.mb.menu=tk.Menu(self.mb,tearoff=False)

        self.mb["menu"]=self.mb.menu
        self.mb.menu.add_command(label="Open", underline=0, command=self.load)
        self.mb.menu.add_command(label="Quit", underline=0, command=self.quit)
        self.mb.pack()

        m1 = PanedWindow(self,showhandle=True,sashrelief=RAISED,height=600)
        m1.pack(fill=BOTH, expand=1)

        left = LabelFrame(m1, text="Algorithm",width=1000,height=500)
        m1.add(left)
        self.algtext=tk.Text(left)

        scr = tk.Scrollbar(left)
        scr.config(command=self.algtext.yview)
        self.algtext.config(yscrollcommand=scr.set)

        scr.pack(side=LEFT,fill=Y) 
        self.algtext.pack(side=LEFT,fill=BOTH,expand=1)
       

        m2 = PanedWindow(m1, orient=VERTICAL,showhandle=True,sashrelief=RAISED)
        m1.add(m2)

        top = LabelFrame(m2, text="PSy Canvas")
        m2.add(top)

        self.c=tk.Canvas(top)
        self.c.pack(side=LEFT,fill=BOTH,expand=1)

        x=10
        y=10
        callWidth=100
        callHeight=30
        calls=[]

        loop=loopWidget(self.c,"loop",x,y,calls)
        call=callWidget(self.c,"call",x+30,y+30)

        #loopWidth=30
        #loopHeight=30
        #callShape=self.c.create_rectangle(x+loopWidth,y+loopHeight,x+loopWidth+callWidth,y+loopHeight+callHeight,outline="red",fill="yellow",activeoutline="blue",width=2)

        #self.c.create_text(x+loopWidth+callWidth/2, y+loopHeight+callHeight/2, text="kern")

        middle = LabelFrame(m2, text="Generated PSy code")
        m2.add(middle)

        self.psytext=tk.Text(middle)

        scr = tk.Scrollbar(middle)
        scr.config(command=self.psytext.yview)
        self.algtext.config(yscrollcommand=scr.set)

        scr.pack(side=LEFT,fill=Y) 
        self.psytext.pack(side=LEFT,fill=BOTH,expand=1)

        bottom = LabelFrame(m2, text="Python")
        m2.add(bottom)

        self.interact=tk.Text(bottom)
        self.interact.pack(side=LEFT,fill=BOTH,expand=1)

        #self.algtext.bind("<Button-3>", self.popup)

    def popup(self,event):
        self.aMenu.post(event.x_root, event.y_root)

    def load(self):
        absfilename=tkFileDialog.askopenfilename(title="Choose an algorithm file",filetypes=[("Algorithm",("*.f90","*.F90")),("All files","*.*")])
        # the parser requires us to be in the same directory. This should be fixed.
        path,filename=os.path.split(absfilename)
        os.chdir(path)
        ast,invokeInfo=parse(filename)
        self.algtext.delete(1.0, END) 
        self.psy=PSy(invokeInfo)
        # *************************************
        # USE invoke object (or some unique text from the invoke) as the tag so each object gets its own callback?
        # need to link invoke objects to line numbers (need to be provided by parser)
        # hacky temporary alternative just count invokes for the moment
        invokeCount=0
        for line in str(ast).split("\n"):
            if "invoke" in line.lower():
                tag="invoke"+str(invokeCount)
                self.algtext.insert(tk.INSERT, line+"\n", tag)
                bind=Bind(self.algtext,tag,self.psy.invokes.invokeList[invokeCount],self.psytext,self.interact,self.c)
                self.algtext.tag_bind(tag,"<Enter>", bind.entry)
                self.algtext.tag_bind( tag, '<Leave>', bind.leave )
                self.algtext.tag_bind( tag, '<ButtonPress-1>', bind.button )
                invokeCount+=1
            else:
                self.algtext.insert(tk.INSERT, line+"\n")

class Bind:
    def __init__(self,text,tag,invoke,psytext,interact,canvas):
        self.canvas=canvas
        self.interact=interact
        self.psytext=psytext
        self._tag=tag
        self._text=text
        self._invoke=invoke
        self.leave(None) # initialise as raised
    def entry(self,event):
        self._text.tag_configure(self._tag, background="orange", foreground="black",borderwidth=2,relief=tk.SUNKEN)
    def leave(self,event):
        self._text.tag_configure(self._tag, background="orange", foreground="black",borderwidth=2,relief=tk.RAISED)
    def button(self,event):
         print "button pressed for invoke "+self._invoke.name
         self.psytext.delete(1.0, END) 
         self.psytext.insert(tk.INSERT,str(self._invoke.gen()))


if __name__ == '__main__':
    app = PSyclone()
    app.pack(fill=tk.BOTH, expand=tk.YES)
    app.master.title('PSyclone')
    app.mainloop()



         #sh = Shell(self.interact)
         #sh.interact()

#class FileCacher:
#    "Cache the stdout text so we can analyze it before returning it"
#    def __init__(self): self.reset()
#    def reset(self): self.out = []
#    def write(self,line): self.out.append(line)
#    def flush(self):
#        output = '\n'.join(self.out)
#        self.reset()
#        return output

#class Shell(InteractiveConsole):
#    "Wrapper around Python that can filter input/output to the shell"
#    def __init__(self,interact):
#        self.interact=interact
#        self.stdout = sys.stdout
#        self.cache = FileCacher()
#        InteractiveConsole.__init__(self)
#        return

#    def get_output(self): sys.stdout = self.cache
#    def return_output(self): sys.stdout = self.stdout

#    def push(self,line):
#        self.get_output()
#        # you can filter input here by doing something like
#        # line = filter(line)
#        InteractiveConsole.push(self,line)
#        self.return_output()
#        output = self.cache.flush()
#        # you can filter the output here by doing something like
#        # output = filter(output)
#        print output # or do something else with it
#        return 

