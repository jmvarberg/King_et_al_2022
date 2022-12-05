//make ROI overlays

#@ File (label = "Tiff Substacks Directory", style="directory") input
#@ File (label = "Overlays Directory", style="directory") overlays



//changes: use file list for substacks, search for files in middleslices that contain that file name, open the image and it's ROI, save that out as a substack into overlays.
roiManager("reset");
substacks = getFileList(input); //get list of original substacks to get base file names
substackArray = newArray(0); //make array to store tif filenames
for(b = 0; b < substacks.length; b++) { //loop through all files in substacks directory, add to array and strip the ".tif" from name
	if(endsWith(substacks[b], ".tif")) {
		label = substring(substacks[b], 0, indexOf(substacks[b], ".tif"));
		substackArray = Array.concat(substackArray, label);
	}
}

Array.show(substackArray);

midslices = getFileList(overlays);
for(a = 0; a < substackArray.length; a++) {
	for (f = 0; f < midslices.length; f++) {
		if(indexOf(midslices[f], substackArray[a]) >= 0) {
			open(overlays+File.separator+midslices[f]);
			
		}
	}
	if(nImages > 1){
	run("Images to Stack", "name=Stack title=[] use");
	saveAs("Tiff...", overlays+File.separator+substackArray[a]+"_combined_ROI_overlays.tif");
	run("Close All");
	}
	else {
		run("Close All");
		continue
	
	}
}
