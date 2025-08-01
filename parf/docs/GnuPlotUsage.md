* Summary: Examples of visualizing PARF results using gnuplot.
* Labels: Phase-Deploy, Featured


# gnuplot Examples

PARF can visualise the data by generating script and data files for [gnuplot](http://www.gnuplot.info/). The main option to this purpose is -g, which specifies the script file to generate, and changes the output of several other options to conform to the input file format usable by gnuplot.

As usual, the --verbose switch can always be left off. Since there will be several examples listed here, let us first generate a forest, and save it for future use:

    parf --verbose -t trainset.arff -fs forest

One of the most interesting visualisations is the data scaling. The graphs can be obtained by issuing the following command:

    parf --verbose -t forest -g script.gnu -st scale.train.dat -sa scale.test.dat

Then, from gnuplot, issue the following command:

    load 'script.gnu'

Very similar effect can be achieved with starting gnuplot with the script as its argument, but in that case gnupot exits at the end of the script:

    gnuplot script.gnu

Also, as long as there are no 3D graphs (because of the bug in gnuplot 4.0), and you only need to see the graphs and do not need to use them again, you can also try the following:

    parf --verbose -t forest -g -st scale.train.dat -sa scale.test.dat | gnuplot -persist

If you want to generate the graphs as image files, use the -gt option. For example:

    parf --verbose -t forest -g script.gnu -st scale.train.dat -sa scale.test.dat -gt jpeg

or (in a bit more complex case):

    parf --verbose -t forest -g script.gnu \
    -st scale.train.dat -sa scale.test.dat \
    -gt 'postscript "VAGRoundedBT_Regular" 14 fontfile "bvrr8a.pfa"'

Using the -gt option generates the set terminal command in the script file. Also, the extension of the resultant image is set to the first word of the argument, except when it is "postscript", in which case the extension is "ps".

It is a useful technique to put both training and test data on the same graph. Follow this procedure to do it in a nice way. First identify the axis ranges; let's say that all points fall between -0.2 and 0.2 on the x axis, and between -0.2 and 0.4 on the y axis (assuming we're doing 2-coordinate scaling; 3D graphs which are shown for 3 or more coordinates is analogous). Open the script file with your favourite editor, and find two plot commands. (or splot for 3D graphs). Just after this word, insert the ranges you identified, like this:

    plot [-0.2:0.2][-0.2:0.4] '-' using 2:3 pt 1 title 'yes', '-' using 2:3 pt 2 title 'no'

Then only in the first one change all pts to 0 (zero). At the beginning of the file, place this command:

    set multiplot

And at the very end of the file (or, as the penultimate command if there is pause at the bottom), insert this command:

    unset multiplot

Then run gnuplot as already described. Admittedly, this could have been integrated into parf, but we feared the explosion of options, if we started allowing every tweak to gnuplot output possible, and decided to leave the (simple) procedures like this in the user's hands. Also, it should not be difficult to devise a Perl (or some other type of) script to do this processing instead of the user. Finally, if there are enough user requests for such a feature, we will try to add it in the future.

<br><sub>Last edited: 2023-05-25 18:29:38</sub>
