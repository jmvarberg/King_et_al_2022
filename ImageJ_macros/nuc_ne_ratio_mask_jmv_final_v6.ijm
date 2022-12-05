//macro to run on image stacks/timelapses to make NE vs. nuc measurements using thresholding/mask on histone channel.

//to do/add/fix:

//1) If macro crashes midway through, re-run, but skip any images that already have been analyzed and are in the results table

//get screen dimensions to set up window positioning
h = screenHeight
w = screenWidth

#@ File (label = "Input Directory", style="directory") input
#@ File (label = "Output Directory", style="directory") output
#@ String (label = "Filetype", value = ".tif") type
#@ Integer (label = "Channel for nucleoplasmic marker", value = 1) nucCh
#@ Integer (label = "Channel for Nup marker", value = 2) nupCh
#@ String (label = "Timepoints to subset (can leave blank)", value = "1-9") tpoints

rois = output + File.separator + "ROIs/";
if (!File.exists(rois)) {
	File.makeDirectory(rois);
}

tifs = output + File.separator + "Middle_slices/";
if (!File.exists(tifs)) {
	File.makeDirectory(tifs);
}

overlays = output + File.separator + "Overlays/";
if (!File.exists(overlays)) {
	File.makeDirectory(overlays);
}

csvs = output + File.separator + "Measurements/";
if (!File.exists(csvs)) {
	File.makeDirectory(csvs);
}

roiManager("reset");
run("Set Measurements...", "area mean integrated redirect=None decimal=2");
//make list of files in input directory, open and prompt to make substack of timepoints to measure

nucArray = newArray(0); //IntDen for optimized nucleoplasmic mask
neArray = newArray(0); //IntDen for NE band
nucMean = newArray(0); //mean nucleoplasmic
neMean = newArray(0); //mean NE
neRatio = newArray(0); //mean NE/mean nucleoplasmic
nucRatio = newArray(0); //mean nucleoplasmic/mean NE
imgArray = newArray(0); //array for image IDs
nucArea = newArray(0); //area of nucleoplasm after convexhull optimization
NEArea = newArray(0); //area of NE band mask

//build in here a check to see if the image is already middle slice, if it is then skip the setup for choosing slice/timepoints

filelist = getFileList(input);
for (j = 0; j < filelist.length; j++) {
    if (endsWith(filelist[j], type)) { 
    	open(input+File.separator+filelist[j]);
    	orig = getTitle();
    	run("Maximize");
    	dupname = substring(orig, 0, indexOf(orig, type));

		//conditionally define middle slices if not already designated
		Stack.getDimensions(width, height, channels, slices, frames);
		if (slices == 1 && frames == 1) {
    		img = dupname;
    		imgArray = Array.concat(imgArray, img);
    		run("Duplicate...", "title=midslice duplicate");
    		run("Split Channels");
    		selectWindow("C"+nucCh+"-midslice");
    		run("Duplicate...", "title=dup");
    		setAutoThreshold("Otsu dark");
			setOption("BlackBackground", true);
			run("Convert to Mask");
			run("Fill Holes");
			run("Close-");

			selectWindow("C1-midslice");
			setLocation(0.1*w, 0.1*h, 0.4*h, 0.4*h);
			
			selectWindow("C2-midslice");
			setLocation((0.1*w)+(0.4*h), 0.1*h, 0.4*h, 0.4*h);

			selectWindow("dup");
			//run("Maximize");
			setLocation((0.1*w)+(0.8*h), 0.1*h, 0.4*h, 0.4*h);

			//user use wand to select ROI of interest
			selectWindow("dup");
			run("Select None");
			setTool("wand");
			waitForUser("Select the ROI of interest. To skip, use rectangle tool to select a black ROI.");

			//once selected, then do the enlarge and convex hull adjustment
			run("Enlarge...", "enlarge=1 pixel");
        	run("Convex Hull");
        	run("Enlarge...", "enlarge=-1 pixel");
        	roiManager("Add"); //add the convex hull roi

			
			//build in here - if want to skip, pick a blank ROI. measure pixel and if 0, then skip/continue.
			run("Measure");
			val = getResult("IntDen", 0);
			if (val == 0) {
				roiManager("reset");
				selectWindow("C"+nupCh+"-midslice");
				run("Duplicate...", "duplicate");
				rename("c2overlay");
				roiManager("show all");
				saveAs("Tiff...", overlays+dupname+"_overlay.tif");
				close();
				run("Clear Results");
				run("Close All");
							
				nucArray = Array.concat(nucArray, 0);
				neArray = Array.concat(neArray, 0);
				nucMean = Array.concat(nucMean, 0);
				neMean = Array.concat(neMean, 0);
				neRatio = Array.concat(neRatio, 0);
				nucRatio = Array.concat(nucRatio, 0);
				nucArea = Array.concat(nucArea, 0);
				NEArea = Array.concat(NEArea,0);
				
				Table.create("Results");
				Table.setColumn("Image", imgArray);
				Table.setColumn("Nucleoplasm.IntDen", nucArray);
				Table.setColumn("NE.IntDen", neArray);
				Table.setColumn("Nucleoplasm.Area", nucArea);
				Table.setColumn("NE.Area", NEArea);
				Table.setColumn("Nucleoplasm.Mean", nucMean);
				Table.setColumn("NE.Mean", neMean);
				Table.setColumn("NE.Nuc.Ratio", neRatio);
				Table.setColumn("Nuc.NE.Ratio", nucRatio);
				Table.save(csvs+"NE_nucleoplasm_ratio_measurements.csv");
				continue
			}
			 else {
				//FIX THIS PART HERE TO MAKE CORRECT MEASUREMENTS AND PUT IN CORRECT ARRAYS. ALSO FIX SECOND SECTION FOR FULL SUBSTACKS TO MATCH NEW METHODS

				
				//get nuclear area measurements before erosion/dilation
				count=roiManager("count");
				if(count != 1) {
					waitForUser("Select desired ROIs");
				}
				roiManager("Select", 0);
				roiManager("Measure");
				area = getResult("Area", 0);
				nucArea = Array.concat(nucArea, area);
				roiManager("reset");
				run("Clear Results");

				//use new methods for defining ROIs and making measurements.
				roiManager("Select", 0); //filled/closed, convex hull opt nucleoplasmic roi
				//generate optimized NE mask using Make Band on optimized convex hull nucleoplasmic mask;
				run("Make Band...", "band=0.4");
				roiManager("Add"); //NE mask ROI is now ROI index 1

				run("Clear Results");
	
				//save ROI masks for each frame, measure on Nup channel. Save to arrays for making output tables
				roiManager("save", rois+dupname+"_masks.zip");
				selectWindow("C"+nupCh+"-midslice");
				run("Duplicate...", "duplicate");
				rename("c2overlay");
				roiManager("show all");
				saveAs("Tiff...", overlays+dupname+"_overlay.tif");
				close();
				selectWindow("C"+nupCh+"-midslice");
				roiManager("Deselect");
				roiManager("multi-measure measure_all");
				roi1 = getResult("IntDen", 0); //IntDens measurement for inner NE mask
				roi2 = getResult("IntDen", 1); //IntDens measurement for dilated outer NE mask
				//roi3 = getResult("IntDen", 2); //IntDens measurement for eroded nucleoplasm
	
				//calculate measurements and save to arrays
				nucArray = Array.concat(nucArray, roi1); //add nucleplasmic ROI2 measurement to nucArray
				
				//calculate NE measurement by subtracting ROI1 from ROI2 and add to array
				ne = roi2-roi1;
				neArray = Array.concat(neArray, ne);
				nerat = ne/roi1;
				neRatio = Array.concat(neRatio, nerat);
				nucrat = roi1/ne;
				nucRatio = Array.concat(nucRatio, nucrat);
				innerA = getResult("Area", 0);
				outerA = getResult("Area", 1);
				innerArea = Array.concat(innerArea, innerA); //area of nucleoplasmic roi
				outerArea = Array.concat(outerArea, outerA); //area of whole nucleus roi


				
				run("Clear Results");
				close("C"+nucCh+"-midslice");
	
				//build in here saving ROIs onto the middle C2 slice and saving with ROIs. Then at end of macro, open all up and combine into a stack.
				//selectWindow("C2-midslice");
				//roiManager("show all without labels");
				//saveAs("Tiff...", overlays+File.separator+dupname+"_frame_"+i+"_middleslice_ROIs_as_overlay.tif");
				close("C2-midslice");
				selectWindow(orig);
				saveAs("Tiff...", tifs+orig);
				close();
				close("dup");
				roiManager("reset");
	
				//make output table
				
				Table.create("Results");
				Table.setColumn("Image", imgArray);
				Table.setColumn("Nuclear.Envelope", neArray);
				Table.setColumn("Nucleoplasm", nucArray);
				Table.setColumn("NE.Nuc.Ratio", neRatio);
				Table.setColumn("Nuc.NE.Ratio", nucRatio);
				Table.setColumn("Nucleoplasmic.Area", innerArea);
				Table.setColumn("WholeNuc.Area", outerArea);
				Table.setColumn("Nuc.Area", nucArea);
				Table.save(csvs+"NE_nucleoplasm_ratio_measurements.csv");
	    		}
	    	}

	    	else {
	    		//use Make Substack to specify timepoints that you want to analyze
		    	if (lengthOf(tpoints) > 0) {
						run("Make Substack...", "frames="+tpoints);
					} else {
						//including this waitforuser because can't navigate through slices/channels in Make Substack window
						waitForUser("Examine stack to choose substack parameters");
						run("Make Substack...");
					}
		    	rename("substack");
		    	Stack.getDimensions(width, height, channels, slices, frames);
				
				//for each timpoint to be analyzed, select middle slice, duplicate out, split channels and use histone channel to make masks for measurements
				
		    	for (i = 1; i <= frames; i++) {
		    		selectWindow("substack");
		    		Stack.setFrame(i);
		    		img = dupname+"_frame_"+i;
		    		imgArray = Array.concat(imgArray, img);
		    		run("Duplicate...", "duplicate frames="+i);
		    		rename("subframe");
		    		run("Maximize");
		    		waitForUser("Choose Middle Slice");
		    		Stack.getPosition(channel, slice, frame);
		    		run("Duplicate...", "duplicate slices="+slice);
		    		rename("midslice");
		    		run("Duplicate...", "duplicate");
		    		rename("middle");
		    		selectWindow("midslice");
		    		run("Split Channels");
		    		selectWindow("C"+nucCh+"-midslice");
		    		run("Duplicate...", "title=dup");
		    		setAutoThreshold("Otsu dark");
					setOption("BlackBackground", true);
					run("Convert to Mask");
					run("Fill Holes");
					run("Close-");
					selectWindow("C1-midslice");
					setLocation(0.1*w, 0.1*h, 0.4*h, 0.4*h);
			
					selectWindow("C2-midslice");
					setLocation((0.1*w)+(0.4*h), 0.1*h, 0.4*h, 0.4*h);

					selectWindow("dup");
					//run("Maximize");
					setLocation((0.1*w)+(0.8*h), 0.1*h, 0.4*h, 0.4*h);
		
					//user use wand to select ROI of interest
					selectWindow("dup");
					setTool("wand");
					waitForUser("Select the ROI of interest. To skip, use rectangle tool to select a black ROI.");
					
					//build in here - if want to skip, pick a blank ROI. measure pixel and if 0, then skip/continue.
					run("Measure");
					val = getResult("IntDen", 0);
					if (val == 0) {
						roiManager("reset");
						selectWindow("C"+nupCh+"-midslice");
						run("Duplicate...", "duplicate");
						rename("c2overlay");
						roiManager("show all");
						saveAs("Tiff...", overlays+dupname+"_frame_"+i+"_overlay.tif");
						close();
						run("Clear Results");
						wins = getList("window.titles");
						for (w=0; w < wins.length; w++) {
							selectWindow(wins[w]);
							if (wins[w] != "subframe") {
								close();
							}
						}
						nucArray = Array.concat(nucArray, 0);
						neArray = Array.concat(neArray, 0);
						neRatio = Array.concat(neRatio, 0);
						nucRatio = Array.concat(nucRatio, 0);
						nucArea = Array.concat(nucArea, 0);
						innerArea = Array.concat(innerArea, 0); //area of nucleoplasmic roi
						outerArea = Array.concat(outerArea, 0); //area of whole nucleus roi
						Table.create("Results");
						Table.setColumn("Image", imgArray);
						Table.setColumn("Nuclear.Envelope", neArray);
						Table.setColumn("Nucleoplasm", nucArray);
						Table.setColumn("NE.Nuc.Ratio", neRatio);
						Table.setColumn("Nuc.NE.Ratio", nucRatio);
						Table.setColumn("Nucleoplasmic.Area", innerArea);
						Table.setColumn("WholeNuc.Area", outerArea);
						Table.setColumn("Nucleus.Area", nucArea);
						Table.save(csvs+"NE_nucleoplasm_ratio_measurements.csv");
						continue
					} else {
						run("Clear Results");
						run("Make Inverse");
						run("Set...", "value=0");
						run("Select None");
						run("Analyze Particles...", "size=1.00-Infinity exclude add");
						setTool("rectangle");
						
						//get nuclear area measurements before erosion/dilation
						count=roiManager("count");
						if(count != 1) {
							waitForUser("Select desired ROIs");
						}
						roiManager("Select", 0);
						roiManager("Measure");
						area = getResult("Area", 0);
						nucArea = Array.concat(nucArea, area);
						roiManager("reset");
						run("Clear Results");
		
						//setup and measure nucleoplasm/NE ROIs. ROI1 = inner mask for NE; ROI2 = outer mask for NE; ROI3 = nucleoplasm mask.
						selectWindow("dup");
						run("Erode");
						//run("Erode");
						run("Analyze Particles...", "size=0.1-Infinity exclude add");
						count=roiManager("count");
						if(count != 1) {
							waitForUser("Fix bad ROIs");
						}
			
						//dilate to get outer NE mask
						selectWindow("dup");
						run("Select None");
						run("Dilate");
						run("Dilate");
						run("Dilate");
						run("Dilate");
						run("Dilate");
						run("Analyze Particles...", "size=1.00-Infinity exclude add");
						count=roiManager("count");
						if(count != 2) {
							waitForUser("Fix bad ROIs");
						}
						
						run("Clear Results");
			
						//save ROI masks for each frame, measure on Nup channel. Save to arrays for making output tables
						roiManager("save", rois+dupname+"_frame_"+i+"_masks.zip");
						selectWindow("C"+nupCh+"-midslice");
						run("Duplicate...", "duplicate");
						rename("c2overlay");
						roiManager("show all");
						saveAs("Tiff...", overlays+dupname+"_frame_"+i+"_overlay.tif");
						close();
						selectWindow("C"+nupCh+"-midslice");
						roiManager("Deselect");
						roiManager("multi-measure measure_all");
						roi1 = getResult("IntDen", 0); //IntDens measurement for inner NE mask
						roi2 = getResult("IntDen", 1); //IntDens measurement for dilated outer NE mask
						//roi3 = getResult("IntDen", 2); //IntDens measurement for eroded nucleoplasm
			
						//calculate measurements and save to arrays
						nucArray = Array.concat(nucArray, roi1); //add nucleplasmic ROI2 measurement to nucArray
						
						//calculate NE measurement by subtracting ROI1 from ROI2 and add to array
						ne = roi2-roi1;
						neArray = Array.concat(neArray, ne);
						nerat = ne/roi1;
						neRatio = Array.concat(neRatio, nerat);
						nucrat = roi1/ne;
						nucRatio = Array.concat(nucRatio, nucrat);
						innerA = getResult("Area", 0);
						outerA = getResult("Area", 1);
						innerArea = Array.concat(innerArea, innerA); //area of nucleoplasmic roi
						outerArea = Array.concat(outerArea, outerA); //area of whole nucleus roi
		
		
						
						run("Clear Results");
						close("C1-midslice");
			
						//build in here saving ROIs onto the middle C2 slice and saving with ROIs. Then at end of macro, open all up and combine into a stack.
						//selectWindow("C2-midslice");
						//roiManager("show all without labels");
						//saveAs("Tiff...", overlays+File.separator+dupname+"_frame_"+i+"_middleslice_ROIs_as_overlay.tif");
						close("C2-midslice");
						selectWindow("middle");
						saveAs("Tiff...", tifs+dupname+"_frame_"+i+"_middleslice.tif");
						close();
						close("subframe");
						close("dup");
						roiManager("reset");
			
						//make output table
						
						Table.create("Results");
						Table.setColumn("Image", imgArray);
						Table.setColumn("Nuclear.Envelope", neArray);
						Table.setColumn("Nucleoplasm", nucArray);
						Table.setColumn("NE.Nuc.Ratio", neRatio);
						Table.setColumn("Nuc.NE.Ratio", nucRatio);
						Table.setColumn("Nucleoplasmic.Area", innerArea);
						Table.setColumn("WholeNuc.Area", outerArea);
						Table.setColumn("Nuc.Area", nucArea);
						Table.save(csvs+"NE_nucleoplasm_ratio_measurements.csv");
			    		}
			    	}
			    	//waitForUser("check");
			    	run("Close All");
			    }
			}
}

//make output table
if (isOpen("Results")){
	selectWindow("Results");
	Table.save(csvs+"NE_nucleoplasm_ratio_measurements.csv");
}
else {
	Table.create("Results");
	Table.setColumn("Image", imgArray);
	Table.setColumn("Nuclear.Envelope", neArray);
	Table.setColumn("Nucleoplasm", nucArray);
	Table.setColumn("NE.Nuc.Ratio", neRatio);
	Table.setColumn("Nuc.NE.Ratio", nucRatio);
	Table.setColumn("Nuc.Area", nucArea);
	Table.save(csvs+"NE_nucleoplasm_ratio_measurements.csv");
}

