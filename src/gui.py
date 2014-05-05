#!/usr/bin/env python
import Tkinter as tk
import tkFileDialog
from parse import parse,ParseError
from psyGen import PSy,GenerationError
import os

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

        # create a popup menu
        self.aMenu = tk.Menu(self, tearoff=0)
        self.aMenu.add_command(label="Undo", command=self.hello)
        self.aMenu.add_command(label="Redo", command=self.hello)

        #self.mainFrame = tk.Frame(self)
        #self.mainFrame.grid(row=0, column=0, sticky="nsew")
        #self.mainFrame.columnconfigure(0, weight=1)
        #self.mainFrame.rowconfigure(0, weight=1)
        self.mainFrame=self

        self.mb=tk.Menubutton(self.mainFrame, text="File")
        self.mb.menu=tk.Menu(self.mb,tearoff=False)
        self.mb["menu"]=self.mb.menu
        self.mb.menu.add_command(label="Open", underline=0, command=self.load)
        self.mb.menu.add_command(label="Quit", underline=0, command=self.quit)
        

        #listbox = tk.Listbox(self.mainFrame)
        #listbox.grid(row=0,column=0)
        #listbox.insert(tk.END, "a list entry")

        self.mb.grid()
        #self.mb.columnconfigure(0, weight=1)
        #self.mb.rowconfigure(0, weight=1)

        self.text=tk.Text(self.mainFrame,width=80)
        self.text.grid(sticky="ns",row=1,column=0)
        self.text.columnconfigure(0, weight=1)
        self.text.rowconfigure(1, weight=1)

        self.text.tag_configure("highlight", background="orange", foreground="black",borderwidth=1,relief=tk.RAISED)
        self.text.tag_configure("normal", background="white", foreground="black")
        self.text.tag_bind('normal',"<Enter>", self.entry_normal)
        self.text.tag_bind( 'normal', '<Leave>', self.leave_normal )
        # attach popup to frame
        self.text.bind("<Button-3>", self.popup)

        scr = tk.Scrollbar(self.mainFrame)
        scr.grid(sticky="ns",row=1,column=1)
        scr.columnconfigure(1, weight=1)
        scr.rowconfigure(1, weight=1)
        scr.config(command=self.text.yview)
        self.text.config(yscrollcommand=scr.set)

    def entry_normal(self,event):
        self.text.tag_configure("normal", background="white", foreground="black",borderwidth=2,relief=tk.SUNKEN)
        print "normal in"
    def leave_normal(self,event):
        self.text.tag_configure("normal", background="white", foreground="black",relief=tk.FLAT)
        print "normal out"
    def entry(self,event):
        self.text.tag_configure("highlight", background="orange", foreground="black",borderwidth=2,relief=tk.SUNKEN)
        print "entry!"

    def leave(self,event):
        self.text.tag_configure("highlight", background="orange", foreground="black",borderwidth=2,relief=tk.RAISED)
        print "exit!"

    def button1(self,event):
        print "button1 pressed!"

    def popup(self,event):
        self.aMenu.post(event.x_root, event.y_root)

    def load(self):
        absfilename=tkFileDialog.askopenfilename(title="Choose an algorithm file",filetypes=[("Algorithm",("*.f90","*.F90")),("All files","*.*")])
        # the parser requires us to be in the same directory. This should be fixed.
        path,filename=os.path.split(absfilename)
        os.chdir(path)
        ast,invokeInfo=parse(filename)
        self.psy=PSy(invokeInfo)
        # *************************************
        # USE invoke object (or some unique text from the invoke) as the tag so each object gets its own callback?
        # need to link invoke objects to line numbers (need to be provided by parser)
        # hacky temporary alternative just count invokes for the moment
        invokeCount=0
        for line in str(ast).split("\n"):
            if "invoke" in line.lower():
                tag="invoke"+str(invokeCount)
                self.text.insert(tk.INSERT, line+"\n", tag)
                bind=Bind(self.text,tag,self.psy.invokes.invokeList[invokeCount])
                self.text.tag_bind(tag,"<Enter>", bind.entry)
                self.text.tag_bind( tag, '<Leave>', bind.leave )
                self.text.tag_bind( tag, '<ButtonPress-1>', bind.button )
                invokeCount+=1
            else:
                self.text.insert(tk.INSERT, line+"\n")

class Bind:
    def __init__(self,text,tag,invoke):
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


app = PSyclone()
app.pack(fill=tk.BOTH, expand=tk.YES)
app.master.title('PSyclone')
app.mainloop()
