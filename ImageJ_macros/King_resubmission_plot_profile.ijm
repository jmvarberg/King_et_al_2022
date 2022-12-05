#@ File (label="Select input directory", style="directory") indir
#@ File (label="Select output directory", style="directory") dir
#@ Integer (label = "Line Width", value=5) lw
#@ Integer (label = "Channel for plot profiles", value=2) lc
#@ Integer (label = "Secondary channel for reference", value=1) oc
#@ String (label = "Profile Channel LUT", choices={"Magenta", "Yellow", "Red", "Green", "Blue", "Grays"}, style="listBox") lut
#@ String (label = "Other Channel LUT", choices={"Magenta", "Yellow", "Red", "Green", "Blue", "Grays"}, style="listBox") lut2
#@ String (label = "Suffx/String for files to be analyzed", value = ".tif") suffix

//macro for automating line profiles for multi ROIs in single image (not timelapse series). Run this on tif files that are already selected for the slice of interest

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
	
filelist = getFileList(indir); 
for (j = 0; j < lengthOf(filelist); j++) {
    if (endsWith(filelist[j], suffix)) { 
        open(indir + File.separator + filelist[j]);
		run("Line Width...", "line="+lw);	
		orig = getTitle();
		dupname = substring(orig, 0, indexOf(orig, suffix));
		setLocation(500,500,1024,1024);
		//set multicolor with the LUTs as specified
		run("Make Composite");
		Stack.setChannel(lc);
		run(lut);
		run("Enhance Contrast", "saturated=0.25");
		Stack.setChannel(oc);
		run(lut2);
		
		//set channel to the channel for line profile measurement
		Stack.setChannel(lc);
		setTool("line");
		roiManager("Show All with labels");
		waitForUser("Draw Line ROI and Add to ROI Manager. Click OK when finished to proceed");
		lines = roiManager("count");
		if (lines != 0) {
			roiManager("Save", roioutdir+dupname+"_ROISet.zip");
			roiManager("Set Line Width", lw);
			//loop through each line ROI and make/save line profiles
			for (i=0; i<lines; i++) {
				roiManager("select", i);
				run("Plot Profile");
				run("export plot jru v1", "save=["+profiledir+dupname+"ROI_"+(i+1)+"_raw_plot_profile.pw2"+"]");
				plot = getTitle();
				run("Clear Results");
				Plot.showValues();
				saveAs("Measurements", CSVdir+dupname+"_ROI_"+(i+1)+"_raw_profile.csv");
				selectWindow(plot);
				run("normalize trajectories jru v1", "normalization=Min_Max");
				rename(plot+"_norm");
				normplot = getTitle();
				run("export plot jru v1", "save=["+profiledir+dupname+"_ROI_"+(i+1)+"_norm_plot_profile.pw2"+"]");
				close(plot);
				close(normplot);
				}
		}
		run("Clear Results");
		close(orig);
		roiManager("reset");
		run("Close All");
    } 
}