# based on http://opencv-python-tutroals.readthedocs.org/en/latest/py_tutorials/py_imgproc/py_template_matching/py_template_matching.html


import cv2 as cv
import numpy as np
from os import listdir
from os.path import isdir, join, isfile, basename


def read_image_as_bw( imgpath ):
    rgb = cv.imread( imgpath )
    return cv.cvtColor( rgb, cv.COLOR_BGR2GRAY )


def list_image_paths( dirpath ):
    extensions = ('.jpg', '.jpeg')
    return [join( dirpath, f ) for f in listdir( dirpath ) if isfile( join( dirpath, f ) ) and f.endswith( extensions )]


def listFolderPaths( folderpath ):
    return [join( folderpath, f ) for f in listdir( folderpath ) if isdir( join( folderpath, f ) )]


def count_images( dirpath ):
    extensions = ('.jpg', '.jpeg')
    return sum( 1 for f in listdir( dirpath ) if isfile( join( dirpath, f ) ) and f.endswith( extensions ) )


class Collector( ):
    def __init__( self, scan, template ):

        # scale the scan and template to half their size to speed up the template matching process
        self.shrinkfactor = 1
        self.threshold = 0.65
        self.orig_scan = scan
        self.scan = self._shrink( scan )
        self.template = self._shrink( template )
        self.comparison_method = cv.TM_CCOEFF_NORMED

        # these are needed for cleaning the match locs and for exporting the final crops
        self.templW, self.templH = self.template.shape[::-1]


    def _shrink( self, img ):
        return cv.resize( img, None, fx=self.shrinkfactor, fy=self.shrinkfactor, interpolation=cv.INTER_AREA )


    # interface to the outside world
    def find_glyphs( self ):
        match_locs = self._find_match_locs( )
        return self._get_ROIs( match_locs )

    # returns coordinates of match locations (cleaned)
    def _find_match_locs( self ):
        print 'running template matching...'
        match_result = cv.matchTemplate( self.scan, self.template, self.comparison_method )
        locs = np.where( match_result >= self.threshold )
        points = zip( *locs[::-1] )
        return self._clean_locs( points )

    # for a single region there are multiple points with high matching-scores,
    # this method gets rid of points which are close enough to each other to
    # be considered as belonging to the same letter
    def _clean_locs( self, points ):
        result = points[:1]
        for pt in points:
            in_neighborhood = False
            for pp in result:
                # if the distance between points is within the dimensions of the template,
                # the points are too close, only one of them is kept
                if abs( pt[0] - pp[0] <= self.templW ) and abs( pt[1] - pp[1] <= self.templH ):
                    in_neighborhood = True
                    break
            if not in_neighborhood:
                result.append( pt )
        return result

    # takes the points of the match result and returns crops of the original scans from those points
    def _get_ROIs( self, points ):
        print 'finding ROIs...'

        foundGlyphs = []

        # scale these back
        points = np.multiply( points, (1 / self.shrinkfactor) )
        templW = self.templW * (1 / self.shrinkfactor)
        templH = self.templH * (1 / self.shrinkfactor)

        for i, pt in enumerate( points ):
            resultimg = self.orig_scan[pt[1]: pt[1] + templH, pt[0]: pt[0] + templW]
            foundGlyphs.append( resultimg )

            print  ( pt[0] + templW, pt[1] + templH )

            print type( pt )
            print tuple( pt )
            pt = tuple( pt )

            cv.rectangle( self.orig_scan, pt, ( pt[0] + templW, pt[1] + templH ), (0, 255, 0), 1 )
            cv.imwrite( "/Users/GaborK/PycharmProjects/Type_Revival_Tool/temptest/out/found.jpg", self.orig_scan )
        return foundGlyphs


class AverageCalculator( ):
    def __init__( self ):
        self.folderpath = None
        self.stateVar = "Ready"
        self.stop = False


    # calculates an average of multiple images by
    def calc_avg_for_glyphs( self, glyphs ):
        result = np.zeros( glyphs[0].shape )
        for glyph in glyphs:
            result = np.add( result, glyph )
        return np.divide( result, len( glyphs ) )


    def setFolderPath( self, folderpath ):
        self.folderpath = folderpath


    def _loadImages( self, folder ):
        img_paths = list_image_paths( folder )
        images = [read_image_as_bw( img_path ) for img_path in img_paths]
        return images

    def _countPreviousResults( self, resultname ):
        return sum( 1 for f in listdir( self.folderpath ) if resultname in f )

    def run( self ):
        if self.folderpath:
            folderpaths = listFolderPaths( self.folderpath )
            for folderpath in folderpaths:
                if not self.stop:
                    self.stateVar = "Generating average for glyph: " + "'" + basename( folderpath ) + "'"
                    resultsavename = basename( folderpath ) + "_avg"
                    resultsavename += str( self._countPreviousResults( resultsavename ) )

                    resultsavepath = join( self.folderpath, resultsavename ) + ".jpg"
                    print resultsavename
                    images = self._loadImages( folderpath )
                    avg = self.calc_avg_for_glyphs( images )
                    cv.imwrite( resultsavepath, avg )

                else:
                    return

        self.stateVar = "Finished"



