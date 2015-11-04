/*

File: VideoView.m

Abstract:   NSOpenGLView subclass that handles the rendering off the movie.

Version: 1.0

© Copyright 2005 Apple Computer, Inc. All rights reserved.

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

#import "VideoView.h"
#import "VideoController.h"
#include <mach/mach_time.h>


@interface VideoView (private)

- (CVReturn)_renderTime:(const CVTimeStamp *)timeStamp;
- (GLenum)_readbackFrameIntoBuffer:(void*)buffer alignment:(int)alignment width:(int)width height:(int)height offsetX:(int)offsetX offsetY:(int)offsetY;
- (OSErr)_exportFrame:(MovieExportGetDataParams *)theParams;

@end

static CVReturn renderCallback(CVDisplayLinkRef displayLink, 
                                                const CVTimeStamp *inNow, 
                                                const CVTimeStamp *inOutputTime, 
                                                CVOptionFlags flagsIn, 
                                                CVOptionFlags *flagsOut, 
                                                void *displayLinkContext)
{
    return [(VideoView*)displayLinkContext _renderTime:inOutputTime];
}

//////////
//
// QTMoovProcs_VideoTrackPropertyProc
// Handle requests for information about the output video data.
//
//////////

static OSErr QTMoovProcs_VideoTrackPropertyProc (void *theRefcon, long theTrackID, OSType thePropertyType, void *thePropertyValue)
{
#pragma unused(theRefcon, theTrackID)

	OSErr	myErr = noErr;
	
	switch (thePropertyType) {
		case movieExportUseConfiguredSettings:
			*(Boolean *)thePropertyValue = true;
			break;
			
		default:
			myErr = paramErr;	// non-zero value means: use default value provided by export component
			break;
	}
	
	return(myErr);
}


//////////
//
// QTMoovProcs_VideoTrackDataProc
// Provide the output audio data.
//
//////////

static OSErr QTMoovProcs_VideoTrackDataProc (void *theRefcon, MovieExportGetDataParams *theParams)
{	
    return [(VideoView*)theRefcon _exportFrame:theParams];
}

						
@implementation VideoView

//--------------------------------------------------------------------------------------------------

- (BOOL)isOpaque
{
    return YES;
}

//--------------------------------------------------------------------------------------------------

- (void)windowChangedScreen:(NSNotification*)inNotification
{
    NSWindow *window = [inNotification object]; 
    CGDirectDisplayID displayID = (CGDirectDisplayID)[[[[window screen] deviceDescription] objectForKey:@"NSScreenNumber"] intValue];

    if((displayID != NULL) && (viewDisplayID != displayID))
    {
	CVDisplayLinkSetCurrentCGDisplay(displayLink, displayID);
	viewDisplayID = displayID;
    }
}


//--------------------------------------------------------------------------------------------------
- (void)prepareOpenGL
{
	CVReturn			    ret;

	lock = [[NSRecursiveLock alloc] init];
	
	// Create CGColorSpaceRef 
	CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
		
	// Create CIContext 
	ciContext = [[CIContext contextWithCGLContext:(CGLContextObj)[[self openGLContext] CGLContextObj]
			    pixelFormat:(CGLPixelFormatObj)[[self pixelFormat] CGLPixelFormatObj]
			    options:[NSDictionary dictionaryWithObjectsAndKeys:
				(id)colorSpace,kCIContextOutputColorSpace,
				(id)colorSpace,kCIContextWorkingColorSpace,nil]] retain];
	CGColorSpaceRelease(colorSpace);
	
	// Create CIFilters 
	colorCorrectionFilter = [[CIFilter filterWithName:@"CIColorControls"] retain];	    // Color filter	
	[colorCorrectionFilter setDefaults];						    // set the filter to its default values
	effectFilter = [[CIFilter filterWithName:@"CIZoomBlur"] retain];		    // Effect filter	
	[effectFilter setDefaults];							    // set the filter to its default values
	compositeFilter = [[CIFilter filterWithName:@"CISourceOverCompositing"] retain];    // Composite filter	
	  	    		
	// Create display link 
	CGOpenGLDisplayMask	totalDisplayMask = 0;
	int			virtualScreen;
	long			displayMask;
	NSOpenGLPixelFormat	*openGLPixelFormat = [self pixelFormat];
	viewDisplayID = (CGDirectDisplayID)[[[[[self window] screen] deviceDescription] objectForKey:@"NSScreenNumber"] intValue];  // we start with our view on the main display
	
	// build up list of displays from OpenGL's pixel format
	for (virtualScreen = 0; virtualScreen < [openGLPixelFormat  numberOfVirtualScreens]; virtualScreen++)
	{
		[openGLPixelFormat getValues:&displayMask forAttribute:NSOpenGLPFAScreenMask forVirtualScreen:virtualScreen];
		totalDisplayMask |= displayMask;
	}
	ret = CVDisplayLinkCreateWithOpenGLDisplayMask(totalDisplayMask, &displayLink);
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowChangedScreen:) name:NSWindowDidMoveNotification object:nil];
	// Set up display link callbacks 
	CVDisplayLinkSetOutputCallback(displayLink, renderCallback, self);
		
	// Setup the timecode overlay
	NSDictionary *fontAttributes = [[NSDictionary alloc] initWithObjectsAndKeys:[NSFont labelFontOfSize:24.0f], NSFontAttributeName,
										    [NSColor colorWithCalibratedRed:1.0f green:0.2f blue:0.2f alpha:0.60f], NSForegroundColorAttributeName,
										    nil];
	timeCodeOverlay = [[TimeCodeOverlay alloc] initWithAttributes:fontAttributes targetSize:NSMakeSize(720.0,480.0 / 4.0)];	// text overlay will go in the bottom quarter of the display
	

}

//--------------------------------------------------------------------------------------------------

- (void)awakeFromNib
{
    OSStatus			    error;
    /* Create QT Visual context */
    NSDictionary	    *attributes = nil;
    attributes = [NSDictionary dictionaryWithObjectsAndKeys:[NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithFloat:720.0], kQTVisualContextTargetDimensions_WidthKey, 
													[NSNumber numberWithFloat:480.0], kQTVisualContextTargetDimensions_HeightKey, nil], 
							    kQTVisualContextTargetDimensionsKey, 
							    [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithFloat:720.0], kCVPixelBufferWidthKey, 
													[NSNumber numberWithFloat:480.0], kCVPixelBufferHeightKey, nil], 
							    kQTVisualContextPixelBufferAttributesKey,
							    nil];
    error = QTOpenGLTextureContextCreate(NULL, 
	(CGLContextObj)[[self openGLContext] CGLContextObj],
	(CGLPixelFormatObj)[[self pixelFormat] CGLPixelFormatObj],
	(CFDictionaryRef)attributes, &qtVisualContext);

}

//--------------------------------------------------------------------------------------------------

- (void)dealloc
{
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    [qtMovie release];
    [colorCorrectionFilter release];
    [effectFilter release];
    [compositeFilter release];
    [timeCodeOverlay release];
    CVOpenGLTextureRelease(currentFrame);
    if(qtVisualContext)
	QTVisualContextRelease(qtVisualContext);
    [ciContext release];
    [super dealloc];
}

//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

- (void)update
{
    [lock lock];
    [super update];
    [lock unlock];
}

//--------------------------------------------------------------------------------------------------

- (void)reshape		// scrolled, moved or resized
{
    needsReshape = YES;	// reset the viewport etc. on the next draw
    
    // if we are not playing, force an immediate draw otherwise it will update with the next frame 
    // coming through. This makes the resize performance better as it reduces the number of redraws
    // espcially on the main thread
    if(!CVDisplayLinkIsRunning(displayLink))	
    {
	[self display];
    }
}

//--------------------------------------------------------------------------------------------------

- (void)drawRect:(NSRect)theRect
{
    [lock lock];    
    NSRect		frame = [self frame];
    NSRect		bounds = [self bounds];
    
    [[self openGLContext] makeCurrentContext];
    if(needsReshape)	// if the view has been resized, reset the OpenGL coordinate system
    {
	GLfloat 	minX, minY, maxX, maxY;

	minX = NSMinX(bounds);
	minY = NSMinY(bounds);
	maxX = NSMaxX(bounds);
	maxY = NSMaxY(bounds);

	[self update]; 

	if(NSIsEmptyRect([self visibleRect])) 
	{
		glViewport(0, 0, 1, 1);
	} else {
		glViewport(0, 0,  frame.size.width ,frame.size.height);
	}
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(minX, maxX, minY, maxY, -1.0, 1.0);

	needsReshape = NO;
    }
    // clean the OpenGL context - not so important here but very important when you deal with transparency
    glClearColor(0.0, 0.0, 0.0, 0.0);	     
    glClear(GL_COLOR_BUFFER_BIT);
    // make sure we have a frame to render    
    if(!currentFrame)
	[self updateCurrentFrame];
    // render the frame
    [self renderCurrentFrame];  
    // flush our output to the screen - this will render with the next beamsync
    glFlush();
    [lock unlock];
}

//--------------------------------------------------------------------------------------------------

- (BOOL)acceptsFirstMouse:(NSEvent *)theEvent
{
    return YES;
}

//--------------------------------------------------------------------------------------------------

- (void)mouseDown:(NSEvent *)theEvent
{
    BOOL	keepOn = YES;
    NSPoint	mouseLoc;
    
    while (keepOn) 
    {
        theEvent = [[self window] nextEventMatchingMask: NSLeftMouseUpMask | NSLeftMouseDraggedMask];
        mouseLoc = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	[self setFilterCenterFromMouseLocation:mouseLoc];
        if ([theEvent type] == NSLeftMouseUp)
			keepOn = NO;
    };
    return;
}

//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

- (void)setQTMovie:(QTMovie*)inMovie
{
    [inMovie retain];
    [qtMovie release];
    qtMovie = inMovie;
    if(qtMovie)
    {
		OSStatus error;
		error = SetMovieVisualContext([qtMovie quickTimeMovie],qtVisualContext);
		SetMoviePlayHints([qtMovie quickTimeMovie],hintsHighQuality, hintsHighQuality);	
		[qtMovie gotoBeginning];
		MoviesTask([qtMovie quickTimeMovie], 0);	//QTKit is not doing this automatically
		movieDuration = [[[qtMovie movieAttributes] objectForKey:QTMovieDurationAttribute] QTTimeValue];
		[self setNeedsDisplay:YES];
    }
    
}

//--------------------------------------------------------------------------------------------------

- (QTTime)currentTime
{
    return [qtMovie currentTime];
}

//--------------------------------------------------------------------------------------------------

- (QTTime)movieDuration
{
    return movieDuration;
}

//--------------------------------------------------------------------------------------------------

- (void)setTime:(QTTime)inTime
{
    [qtMovie setCurrentTime:inTime];
    if(CVDisplayLinkIsRunning(displayLink))
		[self togglePlay:nil];
    [self updateCurrentFrame];
    [self display];
}

//--------------------------------------------------------------------------------------------------

- (IBAction)setMovieTime:(id)sender
{
    [self setTime:QTTimeFromString([sender stringValue])];
}

//--------------------------------------------------------------------------------------------------

- (IBAction)nextFrame:(id)sender
{
    if(CVDisplayLinkIsRunning(displayLink))
		[self togglePlay:nil];
    [qtMovie stepForward];
    [self updateCurrentFrame];
    [self display];
}

//--------------------------------------------------------------------------------------------------

- (IBAction)prevFrame:(id)sender
{
    if(CVDisplayLinkIsRunning(displayLink))
	[self togglePlay:nil];
    [qtMovie stepBackward];
    [self updateCurrentFrame];
    [self display];
}

//--------------------------------------------------------------------------------------------------

- (IBAction)scrub:(id)sender
{
    if(CVDisplayLinkIsRunning(displayLink))
	[self togglePlay:nil];

    // Get movie time, duration
    QTTime currentTime;
    NSTimeInterval sliderTime = [sender floatValue];
    TimeValue tv;
        
    currentTime.timeValue = movieDuration.timeValue * sliderTime;
    currentTime.timeScale = movieDuration.timeScale;
    currentTime.flags = 0;
        
    [qtMovie setCurrentTime:currentTime];
    [self updateCurrentFrame];
    [self display];
}

//--------------------------------------------------------------------------------------------------

- (IBAction)togglePlay:(id)sender
{
    if(CVDisplayLinkIsRunning(displayLink))
    {
		CVDisplayLinkStop(displayLink);
		[qtMovie stop];
    } else {
		[qtMovie play];
		CVDisplayLinkStart(displayLink);
    }
}

//--------------------------------------------------------------------------------------------------

- (IBAction)setFilterParameter:(id)sender
{
    [lock lock];
    switch([sender tag])
    {
	case 0:
	    [colorCorrectionFilter setValue:[NSNumber numberWithFloat:[sender floatValue]] forKey:@"inputContrast"];
	    break;

	case 1:
	    [colorCorrectionFilter setValue:[NSNumber numberWithFloat:[sender floatValue]] forKey:@"inputBrightness"];
	    break;

	case 2:
	    [colorCorrectionFilter setValue:[NSNumber numberWithFloat:[sender floatValue]] forKey:@"inputSaturation"];
	    break;
	    
	case 3:
	    [effectFilter setValue:[NSNumber numberWithFloat:[sender floatValue]] forKey:@"inputAmount"];
	    break;
	    
	default:
	    break;
	    
    }
    [lock unlock];
    if(!CVDisplayLinkIsRunning(displayLink))
	[self display];
}


//--------------------------------------------------------------------------------------------------

- (void)setFilterCenterFromMouseLocation:(NSPoint)where
{
    CIVector	*centerVector = nil;
    
    [lock lock];
    centerVector = [CIVector vectorWithX:where.x Y:where.y];
    [effectFilter setValue:centerVector forKey:@"inputCenter"];
    [lock unlock];
    if(!CVDisplayLinkIsRunning(displayLink))
		[self display];
}

//--------------------------------------------------------------------------------------------------

- (GLenum)_readbackFrameIntoBuffer:(void*)buffer alignment:(int)alignment width:(int)width height:(int)height offsetX:(int)offsetX offsetY:(int)offsetY
{
    // setup
    glPushClientAttrib(GL_CLIENT_PIXEL_STORE_BIT);
    glPixelStorei(GL_PACK_ALIGNMENT, alignment);	
    glPixelStorei(GL_PACK_ROW_LENGTH, 0);
    glPixelStorei(GL_PACK_SKIP_ROWS, 0);
    glPixelStorei(GL_PACK_SKIP_PIXELS, 0);
    // readback
    glReadPixels(offsetX, offsetY, width, height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, buffer);    
    // cleanup
    glPopClientAttrib();
    return glGetError();
}

//--------------------------------------------------------------------------------------------------

- (IBAction)safeFrameToFile:(id)sender
{
    // this demonstrates exporting the current frame by using ImageIO to export a CGImage which is created from CoreImage
    NSSavePanel				*savePanel;
    
    if(CVDisplayLinkIsRunning(displayLink))
	[self togglePlay:nil];

    savePanel = [NSSavePanel savePanel];
    [savePanel setRequiredFileType:@"jpg"];
    if([savePanel runModalForDirectory:nil file:@"MyVideoFrame"] == NSFileHandlingPanelOKButton)
    {
	// create an image destination (ImageIO's way of saying we want to save to a file format)
	// note: public.jpeg denotes that we are saving to JPEG
	CGImageDestinationRef imageDestination = CGImageDestinationCreateWithURL((CFURLRef)[savePanel URL], (CFStringRef)@"public.jpeg", 1, nil);
	if (imageDestination == NULL)
	{
	    NSLog(@"problems creating image destination\n");
	    CFRelease(imageDestination);
	    return;
	}
	CGImageRef renderedImage = [ciContext createCGImage:[effectFilter valueForKey:@"outputImage"] fromRect:[[effectFilter valueForKey:@"outputImage"] extent]];
	// add image to the ImageIO destination (specify the image we want to save)
	CGImageDestinationAddImage(imageDestination, renderedImage, NULL);
	// finalize: this saves the image to the JPEG format as data
	if (!CGImageDestinationFinalize(imageDestination))
	{
	    NSLog(@"problems writing JPEG file\n");
	}
	CFRelease(imageDestination);
	CGImageRelease(renderedImage);

    }
}

//--------------------------------------------------------------------------------------------------

- (OSErr)_exportFrame:(MovieExportGetDataParams *)theParams
{
    if(cancelExport)
	return(userCanceledErr);
    
    if (theParams->requestedTime > movieDuration.timeValue)
	return(eofErr);
		
    NSAutoreleasePool	    *myPool = [[NSAutoreleasePool alloc] init];	// As the export is done in a tight loop it is a good idea to have an 
									// autorelease pool in the render frame call so we don't acumulate 
									// objects over the lengthy progress and therefore fill the memory
    QTTime currentTime;
        
    currentTime.timeValue = theParams->requestedTime;
    currentTime.timeScale = movieDuration.timeScale;
    currentTime.flags = 0;
        
    [qtMovie setCurrentTime:currentTime];
    MoviesTask([qtMovie quickTimeMovie], 0);	//QTKit is not doing this automatically
    // render the frame
    [self updateCurrentFrame];			
    [self display];
    // read the frame from the context into our buffer
    if([self _readbackFrameIntoBuffer:contextPixels alignment:outputAlignment width:outputWidth height:outputHeight offsetX:0 offsetY:0])
    {
	NSLog(@"could not readback image!");
    }
    /*
	The way how the pixels are read back through glReadPixels is flipped to what QuickTime expects. 
	This can easily be worked around by rendering upside down - just switch the minY and maxY in glOrtho.
	But as we display in this example the images on the screen, we don't want to do this for visual purposes and 
	therefore resort to flipping the image by hand.
    */
    int i = outputHeight;
    while(--i > 0)
    {
	memcpy(flippedContextPixels + ((outputHeight - i - 1) * contextRowBytes), contextPixels + (i * contextRowBytes), contextRowBytes);
    }
    
    // end flipping code
    
    // fill the return parameters for the compression
    theParams->actualTime = theParams->requestedTime;
    theParams->dataPtr = (void*)flippedContextPixels;
    theParams->dataSize = (**(outputImageDescription)).dataSize;
    theParams->desc = (SampleDescriptionHandle)outputImageDescription;
    theParams->descType = VideoMediaType;
    theParams->descSeed = 1;
    theParams->actualSampleCount = 1;
    theParams->durationPerSample = currentTime.timeScale / 30;
    theParams->sampleFlags = 0L;
    [myPool release];
    return noErr;
}

//--------------------------------------------------------------------------------------------------

- (IBAction)exportMovie:(id)sender
{
    NSSavePanel				*savePanel;
    MovieExportComponent		myExporter = NULL;
    ComponentDescription		myCompDesc;
    Boolean				myCancelled = false;
    long				trackID;
    OSErr				err = noErr;
    MovieExportGetPropertyUPP		theAudioPropProcUPP = nil;
    MovieExportGetDataUPP		theAudioDataProcUPP = nil;
    TimeScale				audioScale = 0;
    void				*audioRefCon = 0;
    
    if(CVDisplayLinkIsRunning(displayLink))
	[self togglePlay:nil];

     
    savePanel = [NSSavePanel savePanel];
    [savePanel setRequiredFileType:@"mov"];
    if([savePanel runModalForDirectory:nil file:@"MyVideo"] == NSFileHandlingPanelOKButton)
    {
	// Delete existing file
	[[NSFileManager defaultManager] removeFileAtPath:[savePanel filename] handler:nil];
	
	// Export into a Quicktime movie
	myCompDesc.componentType = MovieExportType;
	myCompDesc.componentSubType = 'MooV';
	myCompDesc.componentManufacturer = 'appl';
	myCompDesc.componentFlags = canMovieExportFromProcedures;
	myCompDesc.componentFlagsMask = canMovieExportFromProcedures;

	// open the selected movie export component
	myExporter = OpenComponent(FindNextComponent(NULL, &myCompDesc));
	if (myExporter == NULL)
	{
	    NSLog(@"could not find export compontent !");
	    return;
	}

	// create UPPs for the two app-defined export functions
	MovieExportGetPropertyUPP	theVideoPropProcUPP = NewMovieExportGetPropertyUPP(QTMoovProcs_VideoTrackPropertyProc);
	MovieExportGetDataUPP		theVideoDataProcUPP = NewMovieExportGetDataUPP(QTMoovProcs_VideoTrackDataProc);
	    
	MovieExportAddDataSource(myExporter, VideoMediaType,
						movieDuration.timeScale,    // use the original timescale
						&trackID,
						theVideoPropProcUPP,
						theVideoDataProcUPP,
						self);
						
	// setup audio
	NSArray	    *audioTracks = [qtMovie tracksOfMediaType:QTMediaTypeSound];
	if(audioTracks && ([audioTracks count] > 0))	    //only export audio if tracks are available
	{
	    // we are setting up the audio for pass through
	    err = MovieExportNewGetDataAndPropertiesProcs(myExporter, 
							    SoundMediaType, 
							    &audioScale, 
							    [qtMovie quickTimeMovie],
							    [(QTTrack*)[audioTracks objectAtIndex:0] quickTimeTrack],	// we only use the first audio here
							    0, 
							    movieDuration.timeValue, 
							    &theAudioPropProcUPP, 
							    &theAudioDataProcUPP,
							    &audioRefCon);
	    if (err) 
	    {
		NSLog(@"Can't get audio for export");
	    } else {

		err = MovieExportAddDataSource(myExporter, SoundMediaType, audioScale, &trackID, theAudioPropProcUPP, theAudioDataProcUPP, audioRefCon);
	    }
	}

	MovieExportDoUserDialog(myExporter, NULL, NULL, 0, movieDuration.timeValue, &myCancelled);
	if (myCancelled)
	{
	    NSLog(@"User canceled export dialog");
	    return;
	}
    
	isExporting = YES;
	cancelExport = NO;
	
	OSType              myDataType;
	Handle              myDataRef;

	// create the readback and flipping buffers - see note about flipping in _exportFrame method
	outputWidth = 720;
	outputHeight = 480;
	outputAlignment = 4;
	contextRowBytes = outputWidth * outputAlignment;
	contextPixels = calloc(contextRowBytes * outputHeight, sizeof(char));
	flippedContextPixels = calloc(contextRowBytes * outputHeight, sizeof(char));
		
	// setup the image description for the frame compression
	outputImageDescription = (ImageDescriptionHandle)NewHandleClear(sizeof(ImageDescription));
	(*outputImageDescription)->idSize = sizeof(ImageDescription);
	(*outputImageDescription)->cType = kRawCodecType;
	(*outputImageDescription)->vendor = kAppleManufacturer;
	(*outputImageDescription)->spatialQuality = codecLosslessQuality;
	(*outputImageDescription)->width = outputWidth;
	(*outputImageDescription)->height = outputHeight;
	(*outputImageDescription)->hRes = 72L<<16;
	(*outputImageDescription)->vRes = 72L<<16;
	(*outputImageDescription)->dataSize = contextRowBytes * outputHeight;
	(*outputImageDescription)->frameCount = 1;
	(*outputImageDescription)->depth = 32;
	(*outputImageDescription)->clutID = -1;
	
	// export the video data to the data reference
	QTNewDataReferenceFromCFURL((CFURLRef)[savePanel URL], 0, &myDataRef, &myDataType );
	MovieExportFromProceduresToDataRef(myExporter, myDataRef, myDataType);
	// we are done so lets clean up our room
	free(contextPixels);
	contextPixels = nil;
	free(flippedContextPixels);
	flippedContextPixels = nil;
	DisposeMovieExportGetPropertyUPP(theVideoPropProcUPP);
	DisposeMovieExportGetDataUPP(theVideoDataProcUPP);
	if(theAudioPropProcUPP && theAudioDataProcUPP)
	    MovieExportDisposeGetDataAndPropertiesProcs(myExporter, theAudioPropProcUPP, theAudioDataProcUPP, audioRefCon);
	if(outputImageDescription)
	    DisposeHandle((Handle)outputImageDescription);
	outputImageDescription = NULL;
	CloseComponent(myExporter);
	
	if(!cancelExport)
	{
	    // open the movie in the QuickTime Player
	    [[NSWorkspace sharedWorkspace] openFile:[savePanel filename]];
	}
 	isExporting = NO;
  }
}

//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

- (void)renderCurrentFrame
{
    NSRect		frame = [self frame];
    NSRect		bounds = [self bounds];
    
        
    if(currentFrame)
    {
	CGRect	    imageRect;
	CIImage	    *inputImage;
	CIImage	    *timecodeImage;
	
	// update timecode overlay
	timecodeImage = [timeCodeOverlay getImageForTime:[self currentTime]];
	
	inputImage = [CIImage imageWithCVImageBuffer:currentFrame];

	imageRect = [inputImage extent];
	[colorCorrectionFilter setValue:inputImage forKey:@"inputImage"];
	[effectFilter setValue:[colorCorrectionFilter valueForKey:@"outputImage"] forKey:@"inputImage"];
	[compositeFilter setValue:[effectFilter valueForKey:@"outputImage"] forKey:@"inputBackgroundImage"];
	[compositeFilter setValue:timecodeImage forKey:@"inputImage"];
	// render our resulting image into our context
	[ciContext drawImage:[compositeFilter valueForKey:@"outputImage"] 
			atPoint:CGPointMake((int)((frame.size.width - imageRect.size.width) * 0.5), (int)((frame.size.height - imageRect.size.height) * 0.5)) // use integer coordinates to avoid interpolation
			fromRect:imageRect];
		    
    }
    // housekeeping on the visual context
    QTVisualContextTask(qtVisualContext);
}



//--------------------------------------------------------------------------------------------------

- (BOOL)getFrameForTime:(const CVTimeStamp *)timeStamp
{
    OSStatus error = noErr;
    CFDataRef movieTimeData;

    // See if a new frame is available

    if(QTVisualContextIsNewImageAvailable(qtVisualContext,timeStamp))
    {	    
	    CVOpenGLTextureRelease(currentFrame);
	    QTVisualContextCopyImageForTime(qtVisualContext,
		    NULL,
		    timeStamp,
		    &currentFrame);
			    
	    // In general this shouldn't happen, but just in case...
	    if(error != noErr && !currentFrame)
	    {
		    NSLog(@"QTVisualContextCopyImageForTime: %ld\n",error);
		    return NO;
	    }
	    if(isExporting) // during export call directly
	    {
		[delegate movieTimeChanged:self];
		NSEvent*    event = [[self window] nextEventMatchingMask:(NSKeyDownMask | NSKeyUpMask)
							untilDate:[NSDate distantPast]
							inMode:NSDefaultRunLoopMode
							dequeue:YES];	
		switch ([event type])
		{
		    case NSKeyDown:
		    case NSKeyUp:
			if([event keyCode] == 53)   // user pressed escape
			    cancelExport = YES;
			break;
			
		    default:
			break;
		}
	    } else {
		[delegate performSelectorOnMainThread:@selector(movieTimeChanged:) withObject:self waitUntilDone:NO];
	    }
	    return YES;
    }
    return NO;
}

//--------------------------------------------------------------------------------------------------

- (void)updateCurrentFrame
{
    [self getFrameForTime:nil];    
}

//--------------------------------------------------------------------------------------------------

- (CVReturn)_renderTime:(const CVTimeStamp *)timeStamp
{
    CVReturn rv = kCVReturnError;
    NSAutoreleasePool *pool;
    CFDataRef movieTimeData;
    
    pool = [[NSAutoreleasePool alloc] init];
    if([self getFrameForTime:timeStamp])
    {
	[self drawRect:NSZeroRect]; // refresh the whole view
	rv = kCVReturnSuccess;
    } else {
	rv = kCVReturnError;
    }
    [pool release];
    return rv;
}

//--------------------------------------------------------------------------------------------------

@end
