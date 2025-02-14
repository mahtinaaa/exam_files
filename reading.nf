process READ_DATA {

	tag "import"
	label 'process_high'

	cpus   = 2
    memory = 4.GB

	publishDir "${params.outdir}/preprocess", mode: 'copy'

	container "${ workflow.containerEngine == 'singularity' ?
        'library://lescailab/bigdata/bigdata-rstudio:1.4.0' :
        'ghcr.io/lescai-teaching/bigdata-rstudio:1.4.0' }"
	
	input:
	path tsvfile
	path rscriptfile

	output:
	path "*.rds", emit: dataset

	script:
	"""
	Rscript $rscriptfile \
	$tsvfile
	"""

}