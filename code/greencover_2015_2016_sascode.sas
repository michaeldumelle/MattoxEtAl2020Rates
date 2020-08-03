filename turfy1 'greencover_2015_data.csv';
data turfy1;
	infile turfy1 firstobs = 2 dlm = ',';
	input block trt min $ fert $ month response vgrp;
run;

proc print data = turfy1;
run;


*the block and block*min*fert random effects are so close to zero that they present convergence issues - as a result they were removed;
ods graphics on;
proc mixed data = turfy1;
	class block trt min fert month vgrp;
	model response = min|fert|month / residual outp = predresidy1 ddfm = SAT;
	repeated month / subject = block*min*fert group = vgrp type = arh(1);
	*uncomment the next line if you want the pairwise difference table;
	*lsmeans month*fert*min / diff;
run;
ods graphics off;


*uncomment these next lines if you want the residual output;
/*Proc export data=predresidy1
    outfile="turf_analyzer_y1_resid.csv" 

    dbms=CSV

    replace

    ;

run;*/




filename turfy2 'greencover_2016_data.csv';
data turfy2;
	infile turfy2 firstobs = 2 dlm = ',';
	input block trt min $ fert $ month response vgrp vgrp2;
run;

proc print data = turfy2;
run;


*the block and block*min*fert random effects are so close to zero that they present convergence issues - as a result they were removed;
ods graphics on;
proc mixed data = turfy2;
	class block trt min fert month vgrp;
	model response = min|fert|month / residual outp = predresidy2 ddfm = KR;
	repeated month / subject = block*min*fert group = vgrp type = arh(1);
	*uncomment the next line if you want the pairwise difference table;
	lsmeans month*fert*min / diff;
run;
ods graphics off;


*uncomment these next lines if you want the residual output;
/*Proc export data=predresidy2
    outfile="turf_analyzer_y2_resid.csv" 

    dbms=CSV

    replace

    ;

run;*/
