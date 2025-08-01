* Summary: A quick start for use of PARF, and complete list of available options. 
* Labels: Featured, Phase-Deploy



# Quick Start

Once you have your training data in an ARFF file, you can train a forest by running:

    parf --verbose -t trainset.arff

By default, the program takes the last attribute to be the class. This, by itself, is not useful, as the forest is not used in any way, but you can see (thanks to the --verbose switch, which is entirely optional) that it is working. 

We can try to use the generated forest to classify another dataset (or even the same one, if you don't have another dataset yet):

    parf --verbose -t trainset.arff -a dataset.arff

The dataset to be classified can have the same attribute specification as the one used to train the forest, or it can lack the class attribute and have same all the other attributes. If it is unlabeled, the output will show the classes for all the instances; if it is labeled, only the erroneously classified ones will be shown.

The list of all supported options can be obtained by running:

    parf -h

Wherever there is a file name argument, a dash (-) can be written to signify the standard input or standard output. This is the default where the argument is optional. If a existing filename is specified for an output option, the file will be overwritten without warning.

Wherever there is an attribute argument, the attribute name or number can be used. If an argument contains spaces, it must be quoted. In contrast to the ARFF files, the only valid separator between the items within one option is a comma - surrounding spaces are ignored; also, the entire list should be quoted, and not the individual name. (Note that the quoting employed here is parsed by your shell, and not parf code, and has appropriate semantics). For example:

    parf -t test.arff -fd "test forest.txt" -c 3 -u 'marital status, height'



# Forest Growth Options

    -t file

Deterermines which ARFF file to train from.

    -tv [file]

Outputs the training set out-of-bag votes.

    -tc [file]

Outputs the training set confusion matrix. Each row is one class as labeled in the file; each column is one class as classified by the program. "!NoTag" row contains the rows with unknown class label (?); "!NotCl" column contains the instances that could not be classified because they were taken into bootstrap for every single tree (and were thus never out-of-bag).

    -c attribute

Determines the attribute to be used as class attribute. Instead of the attribute number or name, special values last or new can be used. The former is the default, and takes the last attribute for the class attribute. The latter performs the "unsupervised learning", by creating a new attribute, with values { original, constructed }. The number of the instances will double; half of those will be the original data, while the second half will be constructed by permuting randomly each column of the original data.

    -cp category

Determines the category of the class attribute that will be considered positive. All other categories are considered negative. If not specified, no category shall be singled out as positive.

    -n num

Determines the number of trees to grow. Currently the default is the very low value of 10, which is good for testing, but does not give statistically good results. For serious analyses, significantly more should be used. More trees — more accuracy.

> Note: the default value needs to be checked against the latest library versions.

    -m num

Determines the number of attributes to choose randomly at each node. The best node split is determined only on this subset of attributes. Some experimentation is required for each dataset to determine the best value of this parameter. The default is usually "good enough", and is defined as the square root of the number of used attributes.

    -xs num[%]

Determines the minimum number of instances that a node should have before being considered for splitting. A percentage is the percentage of all instances. The default is 2 — i.e. any node that can be split at all should be analysed for splitting.

    -xr ratio[%]

Determines the minimum split ratio that is worth splitting for. If the node split is that unbalanced or more, the split will not happen. A percentage specifies a percentage of the instances in a node; if the percent sign is not used, then the number specifies a ratio (a number between 0 and 1). The default is 0 — any split that is not degenerate (0 : all) is worth it.

    -b num

Determines the number of categories that form a cutoff point between exhaustive and random category split search ("big" attributes). If the number of categories of an attribute is this or greater, then the random search will be performed, since the exhaustive search space is exponentially dependent on the number of categories. Default is 12, signifying that the exhaustive search will be performed only where 2048 or less combinations of categories are possible.

    -bi num

Determines the number of iterations for the random split search, where used.

    -u(u) attribute,...

Lists the attributes that will be used (or unused) in this run. Overrides the @ignore specifier in the ARFF file. It is invalid to override string attributes as used. All attributes except @ignored are used by default before these options take effect, except when -u switch is employed without -uu, when the default is that no attributes will be used before the option takes effect. No spaces should be included in the argument.

    -w num,...

Specifies the weight override. If this option is used, the program ignores the weights listed in the ARFF file, and uses these values instead. The number of weights must match the number of categories of the class attribute.

    -r num

Seeds the random number generator. If you run the program with the same input files and the same command line options, specifying the same seed, the results should be the same in each run. If this option is not specified, the random number generator is seeded randomly (by the system time), and the results are not repeatable.

    -f num

Specifies the number of passes for filling of missing values. If this value is 0, no missing values will be filled. Otherwise, in the first pass rough fills are calculated. Each subsequent pass obtains fills from proximity values. The default is 1, giving only the one rough fill pass.

    -v num

If this option is specified, after the forest has been generated (with all its missing value filling passes), all but num most important attributes will be ignored, and the forest will be generated again.

    -mv num

This option only makes sense if -v option is used as well, and works as the -m option in the second pass, with only the most important attributes retained. The default is the square root of the number of variables actually used in the second pass.



# Additional Training Analysis Options

    -p [num[%]]

When calculating proximities (which is a prerequisite for some of the following calculations), specifies the number of closest instances to retain for each instance. Depending on the number of instances, the proximity matrix can become so large that the calculation cannot be completed. In such cases the value of this parameter should be reduced. The default is 100% of the instances. As many of the following calculations depend on the proximity matrix, this option does somewhat impact their accuracy; however, as the least proximate cases are dropped, the error thus introduced is not very large.

    -if [file]

Requests the fast calculation of variable importances.

    -i [file]

Requests the full calculation of variable importances.

    -ic [file]

Requests the calculation of case-by-case variable importances.

    -ii [file]

Requests the calculation of variable interactions. Experimental.

    -y [file]

Requests the calculation of class prototypes, and displays them in ARFF format (containing only the prototype centre).

    -ya [file]

Requests the calculation of class prototypes, and displays full prototype information.

    -yn num

Specifies the maximum number of prototypes to calculate for each class. The number of calculated prototypes may be smaller, if the calculated prototypes cover all the instances of the class in the dataset.

    -yp num[%]

Specifies the number of instances to consider "close", for the purpose of deciding whether they are covered by a prototype. Must be less or equal to the value of the -p option. The default is 50% or the value of -p, whichever is smaller.

    -s [num]

Specifies the number of coordinates to scale to. The default is 2.

    -sd [num]

Specifies the limit to consecutive diverging iterations in scaling computation before giving up. The default is 10. If no argument is given, divergence is wholly disallowed (same as -sd 0).

    -st [file]

Requests the calculation of training set scaling data. Each instance will be projected into a lower-dimensional space, suitable for plotting.

    -sa [file]

Requests the calculation of classified set scaling data. Each instance will be projected into a lower-dimensional space, suitable for plotting.

    -o num

Specifies the outlier cutoff value; only the instances with outlier measure above or equal to this value will be displayed in the outlier output. By default, all instances are outputted.

    -ot [file]

Requests the calculation of outlier measure in the training set.

    -oa [file]

Requests the calculation of outlier measure in the classified set.



# Dataset Classification Options

    -a file

Deterermines which ARFF file to classify.

    -av [file]

Outputs the classification set votes.

    -ar [file]

Depending on whether the classification set is labeled or not, this option outputs either the misclassified instances, or the results of the classification for all instances.

    -ac [file]

Outputs the classification set confusion matrix. Each row is one class as labeled in the file; each column is one class as classified by the program. "NoTag" row contains the rows with unknown class label (?). The "NotCl" column is always zero, and is left for compatibility with -tc option output.

    -aa [file]

Outputs the classified set in the ARFF format.

    -ta [file]

Outputs the combined training set and classified set in the ARFF format.



# Forest Handling Options

    -fs prefix

Saves the forest for future classification run. A forest is saved in multiple files, with a common prefix.

    -fl prefix

Loads a forest for classification run. None of the forest growth options, as well as the training analysis options except for the proximity, outlier and scaling ones, are available. Note that all the files generated by the -fs option must be present at the same location, denoted by the prefix.

    -fd [file]

Dumps the forest in a human-readable format.



# Options for gnuplot

    -g [file]

Generates a gnuplot script. The output of several other options is modified for gnuplot data file format compatibility (notably: -i, -if, -ic, -ii and -s). The resulting script can be given as an argument to gnuplot, or loaded from gnuplot's command line with its load command. If the terminal (see -gt) is set to generate files (e.g. -gt png), then the easiest way is to pipe the script into gnuplot. The image files will have the same names as the respective data files, with an appropriate extension; if the data files were not saved into files, the images will have default names. Note: if there are any 3D plots included, do not use the piping method. The gnuplot 4.0 (the current version at the time of development) has a bug, because of which gnuplot does not correctly parse the 3D data from the standard input.

    -gt terminal

Specifies a terminal for gnuplot output. See gnuplot's set terminal command for details. If no terminal is selected, multi-window x11 is used, with a pause at the end.

More examples of gnuplot use can be found in the [GnuPlotUsage.md](https://github.com/efurlanm/ml/blob/master/docs/GnuPlotUsage.md).

<br><sub>Last edited: 2023-05-25 18:29:38</sub>
