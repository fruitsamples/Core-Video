/*

File: VideoView.h

Abstract:   NSOpenGLView subclass that handles the rendering off the movie.

Version: 1.0

� Copyright 2005 Apple Computer, Inc. All rights reserved.

IMPORTANT:  This Apple software is supplied to 
you by Apple Computer, Inc. ("Apple") in 
consideration of your agreement to the following 
terms, and your use, installation, modification 
or redistribution of this Apple software 
constitutes acceptance of these terms.  If you do 
not agree with these terms, please do not use, 
install, modify or redistribute this Apple 
software.

In consideration of your agreement to abide by 
the following terms, and subject to these terms, 
Apple grants you a personal, non-exclusive 
license, under Apple's copyrights in this 
original Apple software (the "Apple Software"), 
to use, reproduce, modify and redistribute the 
Apple Software, with or without modifications, in 
source and/or binary forms; provided that if you 
redistribute the Apple Software in its entirety 
and without modifications, you must retain this 
notice and the following text and disclaimers in 
all such redistributions of the Apple Software. 
Neither the name, trademarks, service marks or 
logos of Apple Computer, Inc. may be used to 
endorse or promote products derived from the 
Apple Software without specific prior written 
permission from Apple.  Except as expressly 
stated in this notice, no other rights or 
licenses, express or implied, are granted by 
Apple herein, including but not limited to any 
patent rights that may be infringed by your 
derivative works or by other works in which the 
Apple Software may be incorporated.

The Apple Software is provided by Apple on an "AS 
IS" basis.  APPLE MAKES NO WARRANTIES, EXPRESS OR 
IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED 
WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY 
AND FITNESS FOR A PARTICULAR PURPOSE, REGARDING 
THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE 
OR IN COMBINATION WITH YOUR PRODUCTS.

IN NO EVENT SHALL APPLE BE LIABLE FOR ANY 
SPECIAL, INDIRECT, INCIDENTAL OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, 
REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF 
THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER 
UNDER THEORY OF CONTRACT, TORT (INCLUDING 
NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN 
IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.

*/ 



#import <Cocoa/Cocoa.h>
#import <QTKit/QTKit.h>
#import <QuartzCore/QuartzCore.h>
#import "TimeCodeOverlay.h"

@interface VideoView : NSOpenGLView
{
    // view related
    NSRecursiveLock			*lock;			    // thread lock to protect the OpenGL rendering from multiple threads
    BOOL				needsReshape;		    // when the view changes its size, set the flag to update the OpenGL context
    id					delegate;

    // movie and visual context
    QTMovie				*qtMovie;		    // the movie in its QTKit representation
    QTTime				movieDuration;		    // cached duration of the movie - just for convenience
    QTVisualContextRef			qtVisualContext;	    // the context the movie is playing in
    CVImageBufferRef			currentFrame;		    // the current frame from the movie
    
    // display link
    CVDisplayLinkRef			displayLink;		    // the displayLink that runs the show
    CGDirectDisplayID			viewDisplayID;

    // filters for CI rendering
    CIFilter				*colorCorrectionFilter;	    // hue saturation brightness control through one CI filter
    CIFilter				*effectFilter;		    // zoom blur filter
    CIFilter				*compositeFilter;	    // composites the timecode over the video
    CIContext				*ciContext;
    
    // timecode overlay
    TimeCodeOverlay			*timeCodeOverlay;    
    
    // for movie export
    BOOL				isExporting;
    BOOL				cancelExport;
    char				*contextPixels;		    // readback buffer for the compression
    char				*flippedContextPixels;	    // another buffer to flip the pixels as we read from the screen
    UInt32				contextRowBytes;
    int					outputWidth;
    int					outputHeight;
    int					outputAlignment;
    ImageDescriptionHandle		outputImageDescription;	    // describes our compression
}

- (void)setQTMovie:(QTMovie*)inMovie;

- (IBAction)setMovieTime:(id)sender;
- (IBAction)nextFrame:(id)sender;
- (IBAction)prevFrame:(id)sender;
- (IBAction)scrub:(id)sender;
- (IBAction)togglePlay:(id)sender;
- (IBAction)setFilterParameter:(id)sender;
- (IBAction)safeFrameToFile:(id)sender;
- (IBAction)exportMovie:(id)sender;

- (void)setFilterCenterFromMouseLocation:(NSPoint)where;

- (void)updateCurrentFrame;
- (void)renderCurrentFrame;

- (QTTime)movieDuration;
- (QTTime)currentTime;
- (void)setTime:(QTTime)inTime;

@end
