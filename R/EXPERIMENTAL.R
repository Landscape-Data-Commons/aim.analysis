## 5/22/2017
## PlotTracking - uses the PlotTracking spreadsheet to populate the dd.  Used whenever the dd has not been updated
##                with the PlotTracking info.

## Checks to make sure that the PLOTID in the PlotTracking spreadsheet occurs in TerraDat (if FINAL_DESIG==TS, then PLOTID must occur in TerraDat), and that  all PLOTIDs of the spreadsheet  occurs in the dd.
## If not, the errors are printed out (but processing continues).  THERE is a stand-alone version of the following processing that performs these checks and allows one to fix the problems before running
## the weighting procedures.  HOWEVER, to ensure that fixes were in fact implemented properly, these checks are retained in this module.


##  path must contain the path(s) of the plot tracking spreadsheet & correspond to the order of dds in dd.src (which should be the same order as in
##                the named list called workinglist.



