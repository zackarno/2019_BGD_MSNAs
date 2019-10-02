
library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fontsize=14]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      tab17 [label = '@@17']
      tab18 [label = '@@18']
      tab19 [label = '@@19']
      tab20 [label = '@@20']
      tab21 [label = '@@21']
      tab22 [label = '@@22']
      tab23 [label = '@@23']
      tab24 [label = '@@24']
      tab25 [label = '@@25']
      tab26 [label = '@@26']
      tab27 [label = '@@27']
      tab28 [label = '@@28']
      tab29 [label = '@@29']
      tab30 [label = '@@30']
      tab31 [label = '@@31']
      tab32 [label = '@@32']
      tab33 [label = '@@33']
      tab34 [label = '@@34']
      tab35 [label = '@@35']
      tab36 [label = '@@36']
      tab37 [label = '@@37']
      tab38 [label = '@@38']
      tab39 [label = '@@39']
      tab40 [label = '@@40']
      tab41 [label = '@@41']


      # edge definitions with the node IDs
      tab4 -> tab9
      tab13 -> tab8-> tab10;
      tab7->tab15->tab11;
      tab13 -> tab7->tab14->tab12;
      tab5->tab12;
      tab18->tab22;
      tab19->tab23;
      tab20->tab24;
      tab25->tab30;
      tab27->tab28;
      tab26->tab29;
      
      tab30->tab31->tab36;
      tab12->tab31->tab36;
      tab24->tab31->tab36;
      
      tab29->tab37->tab34;
      tab11->tab37->tab34;
      tab23->tab37->tab34;
      
      tab28->tab38->tab33;
      tab10->tab38->tab33;
      tab22->tab38->tab33;
      
      tab31->tab35;
      tab47->tab35;
      tab30->tab35;
      
      }

      [1]: 'Took debt to repair shelter'
      [2]: 'made no improvement to shelter'
      [3]: 'went into debt to pay house rent'
      [4]: 'no improvement to shelter because it was not needed'
      [5]: 'no improvement to shelter because they were unable'
      [6]: 'Had some issue with shelter'
      [7]: 'had to purchase shelter materials'
      [8]: 'Did not have to purchase materials'
      [9]: 'snfi structure sub score 1'
      [10]: 'snfi structure sub score 2'
      [11]: 'snfi structure sub score 3'
      [12]: 'snfi structure sub score 4'
      [13]: 'Did make improvement to shelter'
      [14]: 'Went into debt for shelter'
      [15]: 'Did not go into debt for shelter'
      [16]: 'Paid for rent in last 6 months'
      [17]: 'Went into debt due to paying rent'
      [18]: 'Use lpg or kerosene fuel only'
      [19]: 'Use lpg or kerosene and at least one other source'
      [20]: 'Used neither lpg or or kerosene'
      [21]: 'snfi fuel sub 1'
      [22]: 'snfi fuel sub 2'
      [23]: 'snfi fuel sub 3'
      [24]: 'snfi fuel sub 4'
      [25]: 'no lights'
      [26]: 'Only one light'
      [27]: 'Minimum light'
      [28]: 'snfi power sub2'
      [29]: 'snfi power sub3'
      [30]: 'snfi power sub4'
      [31]: '> 1 4'
      [32]: 'sev score snfi 1'
      [33]: 'sev score snfi 2'
      [34]: 'sev score snfi 3'
      [35]: 'sev score snfi 4'
      [36]: 'sev score snfi 5'
      [37]: '>0 3s'
      [38]: 'All 2s'
      [39]: 'All 1s'
      [39]: 'All 4s'
      [40]: 'All 3s'
      [41]: '>0 3s '
      

      
   
      
      ")


