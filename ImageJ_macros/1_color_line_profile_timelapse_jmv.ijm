#@ File (label="Select input directory", style="directory") indir
#@ File (label="Select output directory", style="directory") dir
#@ Integer (label = "Line Width", value=5) lw
#@ Integer (label = "Channel for plot profiles", value=1) lc
#@ String (label = "Channel LUT", choices={"Magenta", "Yellow", "Red", "Green", "Blue", "Grays"}, style="listBox") lut
#@ String (label = "Suffx/String for files to be analyzed", value = ".tif") suffix


//macro for automating line profiles for multi ROIs in single image (not timelapse series).

//select and set up output directory to save images and plot profiles to.


roioutdir=dir+"/ROI_Sets/"; //directory to save the ROIs used for line profiles
if(!File.exists(roioutdir))
	File.makeDirectory(roioutdir);
profiledir=dir+"/Plot_Profiles/"; //directory to save the Raw plot profiles for each line ROI as plot window objects (.pw2 - open in ImageJ to see plots).
if(!File.exists(profiledir))
	File.makeDirectory(profiledir);
CSVdir = dir+"/Plot_ProfileCSVs/"; //directory to save out the CSV data from the plot profiles (to be read as input for downstream analysis in Python).
if(!File.exists(CSVdir))
	File.makeDirectory(CSVdir);
tiffoutdir=dir+"/TIFFs/"; //directory to save out the middle slice images used for making line plot profiles.
if(!File.exists(tiffoutdir))
	File.makeDirectory(tiffoutdir);	

filelist = getFileList(indir); 
for (j = 0; j < lengthOf(filelist); j++) {
    if (endsWith(filelist[j], suffix)) { 
        open(indir + File.separator + filelist[j]);
		run("Line Width...", "line="+lw);	
		orig = getTitle();
		dupname = substring(orig, 0, indexOf(orig, suffix));
		//Select channel and middle slice. Set up for measurements and also save out TIFF image of middle slice for reference later
		run("Duplicate...", "duplicate channels="+lc); //duplicate out channel wanted for line profile analysis
		rename("dupchan");
		close(orig);
		setLocation(100,250,1024,1024);
		run(lut); //set LUT for RGB TIFF output of middle slice
		Stack.getDimensions(width, height, channels, slices, frames);
		for (l=1; l < (frames+1); l++) {
			Stack.setFrame(l);
			waitForUser("Select middle slice and click to continue");
			run("Enhance Contrast", "saturated=0.35");
			run("Duplicate...", "use"); //duplicate out middle slice to use for line profiles
			run("RGB Color");
			rename(dupname + "_frame_"+(l+1)+"_RBG.tif");
			rbgname=getTitle();
			save(tiffoutdir+rbgname); // save out RBG version of scaled middle slice image as TIFF
			close(rbgname);
			run("Duplicate...", "use");
			rename("middle"); 
			setLocation(100,250,1024,1024);
			setTool("line");
			roiManager("reset");
			roiManager("Show All with labels");
			waitForUser("Select ROIs", "Draw Line ROI, add to ROI manager, and click OK when finished to proceed.");
			lines=roiManager("count");
			if (lines == 0) {
				Dialog.create("No line detected");
				Dialog.addMessage("Check 'Skip' to skip remaining frames. Check 'Try Again' to go back to add line");
				Dialog.addCheckbox("Skip", 0);
				Dialog.addCheckbox("Go Back", 1);
				Dialog.show();

				skip=Dialog.getCheckbox();
				back=Dialog.getCheckbox();;

				if (skip == 1) continue;

				if (back == 1) {
					selectWindow("middle");
					waitForUser("Select ROIs", "Draw Line ROI, add to ROI manager, and click OK when finished to proceed.");
					lines=roiManager("count");
					roiManager("select", 0);
					run("Plot Profile");
					run("export plot jru v1", "save=["+profiledir+dupname+"_frame_"+l+"_raw_plot_profile.pw2"+"]");
					plot = getTitle();
					run("Clear Results");
					Plot.showValues();
					saveAs("Measurements", CSVdir+dupname+"_frame_"+l+"_raw_profile.csv");
					selectWindow(plot);
					run("normalize trajectories jru v1", "normalization=Max");
					rename(plot+"_norm");
					normplot = getTitle();
					run("export plot jru v1", "save=["+profiledir+dupname+"_frame_"+l+"_norm_plot_profile.pw2"+"]");
					close(plot);
					close(normplot);
					
				}
			}
			else {
				roiManager("select", 0);
				run("Plot Profile");
				run("export plot jru v1", "save=["+profiledir+dupname+"_frame_"+l+"_raw_plot_profile.pw2"+"]");
				plot = getTitle();
				run("Clear Results");
				Plot.showValues();
				saveAs("Measurements", CSVdir+dupname+"_frame_"+l+"_raw_profile.csv");
				selectWindow(plot);
				run("normalize trajectories jru v1", "normalization=Max");
				rename(plot+"_norm");
				normplot = getTitle();
				run("export plot jru v1", "save=["+profiledir+dupname+"_frame_"+l+"_norm_plot_profile.pw2"+"]");
				close(plot);
				close(normplot);
				close("middle");
				selectWindow("dupchan");	
			}
   		 }
	    run("Clear Results");
		close("dupchan");
		run("Clear Results");
		roiManager("reset");
	}
}

		
		






