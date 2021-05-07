# thwg-2021

This repository contains plots and analysis used for a paper entitled "Accounting for Inheritance: Sibling Disputes and Double-Entry Bookkeeping" at THWG at Virginia Tech on 7 May 2021.

The data for all of the plots comes from the estate of Jan della Faille de Oude and the analysis of the accounts is made possible by the [debkeepr package](https://jessesadler.github.io/debkeepr/index.html).

## Overview of scripts
- Cleaning the data
    - clean-data.R
        - Script to transform the raw csv data with separate pounds, shillings, and pence variables to `deb_lsd` vectors with `debkeepr`. The script also creates transactions and accounts data by groupings of accounts instead of single accounts. This brings together all accounts dealing with the same person or the same branch.
    - time-series-data.R
        - Creates running account data to show the amount of credit or debit in each account for every day from the account's opening to its closing.
- inheritance-1583-12-26.R
    - Creates a bar chart of the paternal, maternal, and sororal inheritance on 26 December 1583, the date of the first balance of the estate.
- opening-closing.R
    - This uses the [packcircles package](https://github.com/mbedward/packcircles) to create an overview of the credits and debits at the opening of the books of the estate (8 November 1582) and at the close of the second book (31 December 1594).
- profits-losses.R
    - This script analyzes and visualizes a single account in the books of the estate of Jan de Oude, the profits and losses account of Jan de Oude's trade from 1579 to the end of 1583. It produces two bar charts that show the credits of the account—the sources of profits—and the debit side of the account—the expenses and distribution of the profits. The script groups accounts together to simplify the bar chart.
- subgraph-branches.R and subgraph-london-inheritance.R
    - These scripts use the account books to create network graphs of the relationships between accounts in the estate of Jan de Oude. Both scripts use [ggraph](https://ggraph.data-imaginist.com) to plot the graphs.
    - subgraph-branches.R uses groups of accounts to provide a slightly simplified view of the movement of capital between the branches of Jan de Oude's trade: Verona, Venice, and London.
    - subgraph-london-inheritance.R takes a closer look at the activities on the account of the branch of London in the fall of 1594 to show the last disbursements of inheritance recorded in the account books.
- time-series-inheritance.R
    - This script uses the data from time-series-data.R
    - It creates four different plots, three of which are found in the paper, on the running value of the inheritance accounts of the nine heirs.
    - A full overview of the period of the account books is made using the [gghighlight package](https://yutannihilation.github.io/gghighlight/index.html).
    - The script also creates faceted plots of the opening period of the estate (1582-1584) and its closing period (fall 1594), since no transactions were recorded in the account books between March 1585 and September 1594.
