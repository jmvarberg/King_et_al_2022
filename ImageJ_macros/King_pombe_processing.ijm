//run("Channels Tool...");
outdir = getDirectory("Choose output directory");
orig=getTitle();
duporig=substring(orig, 0, lengthOf(orig)-4);
run("Split Channels");
selectWindow("C1-"+orig);
run("Subtract Background...", "rolling=25 stack"); //change as needed if channel order changes: want 10 for SPB channel, 25 for Nup channel
run("Magenta");
selectWindow("C2-"+orig);
run("Subtract Background...", "rolling=10 stack"); //change as needed if channel order changes: want 10 for SPB channel, 25 for Nup channel
run("Yellow");
run("Merge Channels...", "c4=C3-"+orig+" c6=C1-"+orig+" c7=C2-"+orig+" create");
merged=getTitle();
selectWindow(merged);
run("Z Project...", "projection=[Max Intensity] all");
makeRectangle(133, 33, 80, 80);
waitForUser("Select ROIs");
count=roiManager("count");
roiManager("save", outdir+duporig+"_ROIs.zip");
for (i = 0; i < count; i++) {
	selectWindow(merged);
	roiManager("select", i);
	run("Duplicate...", "duplicate");
	saveAs("Tiff..", outdir+duporig+"_roi_"+(i+1)+"_full_stack.tif");
	close();
	selectWindow("MAX_"+merged);
	roiManager("select", i);
	run("Duplicate...", "duplicate");
	saveAs("Tiff..", outdir+duporig+"_roi_"+(i+1)+"_max_proj.tif");
	close();
}
roiManager("reset");
