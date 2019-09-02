from os import mkdir
from os.path import join, splitext, exists

import cv2 as cv

from glyphcollector.tools import Collector, read_image_as_bw, list_image_paths, count_images


class CollectionHandler( ):
    def __init__( self ):
        self.stateVar = "Ready"
        self.scans_dirpath = None
        self.templates_dirpath = None
        self.output_path = None
        self.stop = False
        self.testRun = False

    def set_scans_dirpath( self, scans_dirpath ):
        self.scans_dirpath = scans_dirpath


    def set_templates_dirpath( self, templates_dirpath ):
        self.templates_dirpath = templates_dirpath


    def set_output_path( self, output_path ):
        self.output_path = output_path


    def _save_results( self, results, outdir, _filename ):
        print 'saving results', outdir
        count = count_images( outdir )
        for i, result in enumerate( results ):
            filename = _filename + str( count + i ) + '.jpg'
            cv.imwrite( outdir + '/' + filename, result )

    def _get_filename( self, path ):
        return splitext( path )[0].split( '/' )[-1]

    def _make_outdir( self, name ):
        outdir = join( self.output_path, name )
        if not exists( outdir ):
            mkdir( outdir )
        return outdir


    def toggleTestRun( self ):
        self.testRun = not self.testRun


    def run( self ):
        self.stop = False
        if self.scans_dirpath and self.templates_dirpath:
            self.stateVar = 'loading resources...'
            scanfilepaths = list_image_paths( self.scans_dirpath )
            templatefilepaths = list_image_paths( self.templates_dirpath )

            if self.stop:
                self.stateVar = "Stopped"
                return

            if self.testRun:
                templatefilepaths = templatefilepaths[:1]

            for scanfile in scanfilepaths:
                scan = read_image_as_bw( scanfile )
                scanfileName = self._get_filename( scanfile )
                for templatefile in templatefilepaths:
                    if self.stop:
                        self.stateVar = "Stopped"
                        return

                    templatefileName = self._get_filename( templatefile )
                    self.stateVar = 'processing scan \'' + scanfileName + '\' with glyph \'' + templatefileName + ' \''

                    template = read_image_as_bw( templatefile )

                    glyphs = Collector( scan, template ).find_glyphs( )

                    if self.stop:
                        self.stateVar = "Stopped"
                        return

                    outdir = self._make_outdir( templatefileName )
                    self._save_results( glyphs, outdir, templatefileName )

            self.stop = True
            self.stateVar = "Finished"




