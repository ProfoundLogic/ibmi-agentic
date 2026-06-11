      * Group
     O          E            headGrp        0
     O                                              'GROUP:'
     O                       saveFDFND     Z   +001
     O                       saveFDFNAM        +002
      * Selection criteria
     O          E            heada          1
     O                                          187 'As of date:'
     O                       saveFDASOF        +001 '  /  /    '
     O          E            heada          1
     O                                          187 'Invoice date:'
     O                       saveFDIVDT        +001 '  /  /    '
      * Employer info
     O          E            headEmpY       1
     O                                              'Employer:'
     O                       saveFDEMP#    z   +001
     O                       saveFDDIV     3   +001
     O                       saveFDFYR     z   +001
     O                       saveFDENAM        +001
     O          E            headEmp        1
     O                                              'Employer:'
     O                       saveFDEMP#    z   +001
     O                       saveFDDIV     3   +001
     O                       saveFDENAM        +001
     O          E            headCap        1
     O                                              'Carrier Policy #:'
     O                       saveFDCAP#        +001
      * Fee type information
     O          E            headb       1  1
     O                                              'Fee Type:'
     O                       xxFeeDesc         +001
     O          E            headb          1
      *      FIDESC + ' Based on:  ' + GTDESC
     O                       xxFeeDescHdg
     O          E            headMin        0
     O                                              'Minimum Fee:'
     O                       saveFDMINF        +001
