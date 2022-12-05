//macro to re-run on existing middle slices with ROI overlays to improve masking.

#@ File (label = "Overlay Directory", style="directory") input
#@ File (label = "Output Directory", style="directory") output
#@ Boolean (label = "Fill Holes/Close- ?", value=0) extra 

nucArray = newArray(0); //IntDen for optimized nucleoplasmic mask
neArray = newArray(0); //IntDen for NE band
nucMean = newArray(0); //mean nucleoplasmic
neMean = newArray(0); //mean NE
neRatio = newArray(0); //mean NE/mean nucleoplasmic
nucRatio = newArray(0); //mean nucleoplasmic/mean NE
imgArray = newArray(0); //array for image IDs
nucArea = newArray(0); //area of nucleoplasm after convexhull optimization
NEArea = newArray(0); //area of NE band mask

filelist = getFileList(input) 
print(filelist.length);
for (i = 0; i < lengthOf(filelist); i++) {
    if (endsWith(filelist[i], "overlay.tif")) { 
        open(input + File.separator + filelist[i]);
        dupname=substring(filelist[i], 0, lengthOf(filelist[i])-4);
        run("To ROI Manager");
        count = roiManager("count");
        if (count != 2) {
        	print(filelist[i] + " does not have ROIs saved"); //add zeros to arrays as place holders
        	imgArray = Array.concat(imgArray, filelist[i]);
			nucArray = Array.concat(nucArray, 0);
			neArray = Array.concat(neArray, 0);
			nucMean = Array.concat(nucMean, 0);
			neMean = Array.concat(neMean, 0);
			nucArea = Array.concat(nucArea, 0);
			NEArea = Array.concat(NEArea, 0);
			neRatio = Array.concat(neRatio, 0);
			nucRatio = Array.concat(nucRatio, 0);
        	run("Close All");
        	
        } else {
        	print(filelist[i] +" ROIs are good");
        	roiManager("Select", 0);
        	roiManager("Rename", "nuc_orig");
        	roiManager("Select", 1);
        	roiManager("Rename", "ne_orig");
        }

		if (extra == 1) {
			roiManager("Select", "nuc_orig");
			run("Create Mask");
			setOption("BlackBackground", true);
			run("Dilate");
			run("Fill Holes");
			run("Close-");
			run("Erode");
			run("Analyze Particles...", "add");
			roiManager("Select", 2);
			roiManager("Rename", "nuc_fill");
			roiManager("Select", 0);
			roiManager("delete");
			roiManager("Select", 1);
			roiManager("Rename", "nuc");
			close("Mask");
			
		}
		
        //fix nucleoplasmic mask by enlarging and convex hull
        roiManager("Select", 1);
        run("Enlarge...", "enlarge=1 pixel");
        run("Convex Hull");
        run("Enlarge...", "enlarge=-1 pixel");
        roiManager("Add");

        //optimized convex hull nucleoplasmic ROI is now ROI 3 (or 2 using 0-based selection)

        //generate optimized NE mask using Make Band on optimized convex hull nucleoplasmic mask
		roiManager("Select", 2);
		run("Make Band...", "band=0.4");
		roiManager("Add");

		//delete old mask ROIs and save out new overlay TIFF
		roiManager("select", 0);
		roiManager("delete");
		roiManager("select", 0);
		roiManager("delete");

		//generate shrunk nucleoplasmic mask to avoid measuring signal at the NE/periphery
		roiManager("select", 0);
		run("Enlarge...", "enlarge=-1 pixel");
		roiManager("Update");

		roiManager("show all with labels");
		saveAs("Tiff...", output+File.separator+dupname+"_optimized_masks_overlay.tif");

		//now, redo the measurements for the two regions and save out new measurements file
			
		run("Set Measurements...", "area mean integrated redirect=None decimal=2");

		roiManager("Deselect");
		roiManager("multi-measure measure_all");

		nucmean = getResult("Mean", 0);
		nemean = getResult("Mean", 1);
		nucarea = getResult("Area", 0);
		nearea = getResult("Area", 1);
		nucID = getResult("IntDen", 0);
		neID = getResult("IntDen", 1);
		ner = nemean/nucmean;
		nur = nucmean/nemean;

		imgArray = Array.concat(imgArray, filelist[i]);
		nucArray = Array.concat(nucArray, nucID);
		neArray = Array.concat(neArray, neID);
		nucMean = Array.concat(nucMean, nucmean);
		neMean = Array.concat(neMean, nemean);
		nucArea = Array.concat(nucArea, nucarea);
		NEArea = Array.concat(NEArea, nearea);
		neRatio = Array.concat(neRatio, ner);
		nucRatio = Array.concat(nucRatio, nur);

		roiManager("reset");
		run("Clear Results");
		run("Close All");

    } 
}

//create and save output table

Table.create("Measurements");
Table.setColumn("Image", imgArray);
Table.setColumn("Nucleoplasm.IntDen", nucArray);
Table.setColumn("NE.IntDen", neArray);
Table.setColumn("Nucleoplasm.Area", nucArea);
Table.setColumn("NE.Area", NEArea);
Table.setColumn("Nucleoplasm.Mean", nucMean);
Table.setColumn("NE.Mean", neMean);
Table.setColumn("NE.Nuc.Ratio", neRatio);
Table.setColumn("Nuc.NE.Ratio", nucRatio);

Table.save(output+File.separator+"Optimized_NE_nuc_ratio_measurements.csv");