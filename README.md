HouseScan
=========

A [distinguished](http://www3.imperial.ac.uk/computing/teaching/ug/ug-distinguished-projects) Computing Master's project at [Imperial College London](http://imperial.ac.uk), 2014. Supervised by Prof. [Andrew Davison](http://www.doc.ic.ac.uk/~ajd/). Written in Haskell. [Read the thesis](https://github.com/nh2/housescan/releases/download/presentation/HouseScan.pdf) or [watch a video](https://github.com/nh2/housescan/releases/download/video/flythrough-highres-hd.avi) (Imperial College video link [here](http://www.doc.ic.ac.uk/~nh910/flythrough-highres-hd.avi)).

### Building-scale interior 3D reconstruction with KinectFusion

You can use HouseScan to create a 3D model of your house, flat, or anything that has rooms aligned in a grid.


# Usage

* Scan each room with my version of KinFu from my [PCL fork](https://github.com/nh2/pcl/tree/niklas-experiments)
* Perform plane detection with the the plane detection tool I added to that fork
* Import the rooms into the HouseScan, arrange them
* Export them in full resolution as `.ply` files
* Load them all into [Meshlab](http://meshlab.sourceforge.net) to see the result

You can find detailed instructions in the appendix of my thesis (linked below), section *A HouseScan user manual*.


# Prerequisites

For a full scan you need:

* An RGB-D camera: Asus Xtion Pro, [Microsoft Kinect](http://en.wikipedia.org/wiki/Kinect), or similar camera working with [OpenNI](http://en.wikipedia.org/wiki/OpenNI)
* A computer with a fast Nvidia graphics card supporting CUDA (preferably a laptop for carrying around) - I used a GTX 780M
* Linux installed (preferably Ubuntu 14.04 or equivalent)
 

# License

HouseScan is [MIT](http://en.wikipedia.org/wiki/MIT_License) licensed. PCL/KinFu are [BSD](http://en.wikipedia.org/wiki/BSD_licenses) licensed. You can use them free of charge, but please tell me when you do something cool with it!
