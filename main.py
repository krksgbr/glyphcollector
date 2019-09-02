from Tkinter import *
import tkFileDialog
import tkMessageBox
import threading

from glyphcollector.collection_handler import CollectionHandler
from tools import AverageCalculator, count_images, listFolderPaths


class App:

    def __init__( self, parent ):

        self.stateVar = "Ready"
        self.collectionHandler = CollectionHandler( )
        self.averageCalculator = AverageCalculator( )
        self.root = parent
        self.root.title( "GlyphCollector" )
        self.root.createcommand( 'tkAboutDialog', self.showAbout )
        self.root.after( 50, self.updateStateLabel )
        self.root.protocol( 'WM_DELETE_WINDOW', self.killProcessAndQuit )

        self.collectionThread = None
        self.avgThread = None

        self.stateLabelVar = StringVar( )
        self.stateLabelVar.set( self.stateVar )

        self.scans_path_selected = False
        self.templates_path_selected = False
        self.output_path_selected = False

        self.scanpathentry = None
        self.templatespathentry = None
        self.outputpathentry = None

        self.scansPathVar = StringVar( )
        self.templatesPathVar = StringVar( )
        self.outputPathVar = StringVar( )

        self.init_widgets( )

    def showAbout( self ):
        info = "GlyphCollector was written by Gabor Kerekes"
        tkMessageBox.showinfo( "About", info )


    def init_widgets( self ):


        def scrollToEnd( entry ):
            entry.icursor( END )
            entry.xview_moveto( 1.0 )

        topFrame = Frame( self.root )
        bottomFrame = Frame( self.root )
        statusFrame = Frame( self.root, bg='grey' )

        topFrame.grid( column=0, row=0, padx=20, pady=20, sticky=W )
        bottomFrame.grid( column=0, row=1, padx=20, pady=20, sticky=W )
        statusFrame.grid( column=0, row=2, sticky=W + E + S )


        ############ SCANS ##############

        Label( topFrame, text="Scans directory:" ).grid( row=0, column=0, sticky=W )

        self.scanpathentry = Entry( topFrame, bg="grey", width=50, textvariable=self.scansPathVar )
        self.scanpathentry.grid( padx=3, row=0, column=2, sticky=W )

        Button( topFrame, text="...", command=self.select_scans_path ).grid( row=0, column=3, sticky=W )



        ############ TEMPLATES ##############

        Label( topFrame, text="Templates directory:" ).grid( row=1, column=0, sticky=W )
        self.templatespathentry = Entry( topFrame, bg="grey", width=50, textvariable=self.templatesPathVar )
        self.templatespathentry.grid( padx=3, row=1, column=2, sticky=W )
        Button( topFrame, text="...", command=self.select_templates_path ).grid( row=1, column=3, sticky=W )



        ############ OUTPUT ##############

        Label( topFrame, text="Output directory:" ).grid( row=2, column=0, sticky=W )
        self.outputpathentry = Entry( topFrame, bg="grey", width=50, textvariable=self.outputPathVar )
        self.outputpathentry.grid( padx=3, row=2, column=2, sticky=W )
        Button( topFrame, text="...", command=self.select_output_path ).grid( row=2, column=3, sticky=W )

        Checkbutton( topFrame, text="Test run", command=self.collectionHandler.toggleTestRun ) \
            .grid( row=3, column=0, sticky=W, pady=10 )

        ############ CONTROL ##############

        Button( bottomFrame, text="Collect Glyphs", command=self.init_collection_process ) \
            .grid( row=0, column=0, sticky=W + E )

        Button( bottomFrame, text="Calculate Averages", command=self.init_average_calculation_process ) \
            .grid( row=0, column=1, sticky=W + E )

        Button( bottomFrame, text="Cancel", command=self.killProcess ) \
            .grid( row=0, column=2, sticky=W + E )


        ############ STATUS ##############
        Label( statusFrame, text="Status:", bg='grey', ).grid( row=0, column=0, sticky=W, pady=5, padx=20 )
        Label( statusFrame, textvariable=self.stateLabelVar, bg='grey', justify=LEFT ) \
            .grid( row=0, column=1 )


    def updateStateLabel( self ):

        if self.collectionThread and self.collectionThread.isAlive( ):
            self.stateLabelVar.set( self.collectionHandler.stateVar )

        elif self.avgThread and self.avgThread.isAlive( ):
            self.stateLabelVar.set( self.averageCalculator.stateVar )

        else:
            self.stateLabelVar.set( "Ready" )

        self.root.after( 50, self.updateStateLabel )


    def select_scans_path( self ):
        dirname = tkFileDialog.askdirectory( parent=self.root, title="select scans directory", mustexist=False )
        if dirname:
            self.scans_path_selected = True
            self.scansPathVar.set( dirname )
            print 'scans directory:', dirname
            self.scanpathentry.xview_moveto( 1.0 )


    def select_templates_path( self ):
        dirname = tkFileDialog.askdirectory(
            parent=self.root,
            title="select templates directory",
            mustexist=False )

        if dirname:
            self.templates_path_selected = True
            self.templatesPathVar.set( dirname )
            print 'templates directory:', dirname
            self.templatespathentry.xview_moveto( 1.0 )

    def select_output_path( self ):
        dirname = tkFileDialog.askdirectory(
            parent=self.root,
            title="select output directory",
            mustexist=False )

        if dirname:
            self.output_path_selected = True
            self.outputPathVar.set( dirname )
            print 'output directory:', dirname
            self.outputpathentry.xview_moveto( 1.0 )


    def init_collection_process( self ):
        scansPath = self.scansPathVar.get( )
        templatesPath = self.templatesPathVar.get( )
        outputPath = self.outputPathVar.get( )

        if scansPath and templatesPath and outputPath:
            self.collectionHandler.set_output_path( outputPath )

            if count_images( scansPath ) > 0:
                self.collectionHandler.set_scans_dirpath( scansPath )

                if count_images( templatesPath ) > 0:
                    self.collectionHandler.set_templates_dirpath( templatesPath )

                    self.collectionThread = threading.Thread( target=self.collectionHandler.run )
                    self.collectionThread.start( )

                else:
                    tkMessageBox.showerror( "Error", "The templates folder doesn't seem to contain any JPEGs." )
            else:
                tkMessageBox.showerror( "Error", "The scans folder doesn't seem to contain any JPEGs." )
        else:
            tkMessageBox.showerror( "Error", "Please specify all folders." )


    def init_average_calculation_process( self ):
        if not self.collectionThread or not self.collectionThread.isAlive( ):
            outputPath = self.outputPathVar.get( )
            if outputPath:
                if listFolderPaths( outputPath ):

                    self.averageCalculator.setFolderPath( outputPath )

                    self.avgThread = threading.Thread( target=self.averageCalculator.run )
                    self.avgThread.start( )

                else:
                    tkMessageBox.showerror( "Error",
                                            "In order to calculate averages, the 'Output directory' needs to contain the results of the collection process." )
            else:
                tkMessageBox.showerror( "Error", "Please specify the output folder." )
        else:
            print "collection is running"


    def killProcess( self ):

        if self.collectionThread and self.collectionThread.isAlive( ):
            self.collectionHandler.stateVar = "Stopped"
            self.collectionHandler.stop = True

        elif self.avgThread and self.avgThread.isAlive( ):
            self.averageCalculator.stateVar = "Stopped"
            self.averageCalculator.stop = True


    def killProcessAndQuit( self ):
        self.killProcess( )
        self.root.quit( )


def main( ):
    root = Tk( )
    App( root )
    root.resizable( 0, 0 )
    root.mainloop( )


main( )













