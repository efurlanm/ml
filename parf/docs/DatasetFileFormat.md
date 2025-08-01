* Summary: PARF uses a modified ARFF format for the datasets, mostly compatible with Weka. 
* Labels: Phase-Deploy, Featured


# Introduction

PARF uses a modified [ARFF](http://www.cs.waikato.ac.nz/~ml/weka/arff.html) format for the datasets, which is mostly compatible with the original format used by [Weka](http://www.cs.waikato.ac.nz/ml/weka/) project.

In short: The file starts with the relation name, which is followed by the specification of attributes, and finally comes the data block. An attribute specification gives the attribute its name, and possible values (either numeric, string, or a set of categories in curly braces). Wherever a value contains a space, it must be quoted. Example:

    % this is an example parf arff file
    @relation test
    @attribute name string
    @attribute height numeric
    @attribute sex { male, female }
    @attribute "marital status" { married (6) "not married" divorced widowed (1.2) }
    @data
    "Anne Appleby", 162, female, married
    "Bob Barnes" 181.5 male "not married"
    'Charlie Coombs', 175, ?, divorced
    "Dan D'Angelo", ?, male, widowed
    "Ellen Ellis", 167.5, "female", "widowed"
    "Francis Ford", &
      165, female, married
    "George Gorsky", ?, ?, ?
    ?, 200, male, ?
    Iris, 159, female, ?

    
# Notable differences from ARFF

-   Only numeric, string and nominal attributes are supported - date is
    not a valid attribute type.
-   Wherever the ARFF syntax requires a comma, a whitespace works as
    well. Also, in each such place, an ampersand (&) can be placed to
    continue in the next line.
-   The line continuation token above is particularly important in wide
    datasets, since line length is limited for technical reasons, and
    set to 1024 characters.
-   Both single and double quotes work. If a single quotation mark is to
    be included in a string, use the double quotes for the string, and
    vice versa.
-   Sparse ARFFs are not supported.
-   String variables are ignored in computation. Additional ways to make
    an attribute ignored is to change @attribute into @ignored, and by
    specifying used/unused attributes on the command line (-u\[u\]
    switch)
-   Nominal attributes can specify category weights in parentheses
    immediately following the category name. The default weight, if not
    specified, is 1. The weights are only used for the attribute that is
    selected to be the class attribute.

<br><sub>Last edited: 2023-05-25 18:29:38</sub>
