     hdatedit(*MDY) datfmt(*MDY)

     fsdcmp     uf   e             disk
     fsmarp     uf   e             disk
     fsmhdp     uf   e             disk
     fsmh1p     uf   e             disk
     fqsysprt   o    f  132        printer oflind(*in71)

     dWorkStor         ds                  inz                                   Work/Storage area
     d  inCTCM                        9p 0                                         SDCMP in
     d  inCTAR                        9p 0                                         SMARP in
     d  inCTHD                        9p 0                                         SMHDP in
     d  inCTH1                        9p 0                                         SMH1P in
     d  upCTCM                        9p 0                                         SDCMP updated
     d  upCTAR                        9p 0                                         SMARP updated
     d  upCTHD                        9p 0                                         SMHDP updated
     d  upCTH1                        9p 0                                         SMH1P updated
     d  xxDATE                        8s 0                                         Current date
     d  xxTIME                        6s 0                                         Current time

     dOldKey           ds                  inz                                   Old Key
     d  oldCO#                 1      3a                                           CO#  3
     d  oldFND                 4      6a                                           FND  3
     d  oldEMP#                7     11a                                           EMP# 5
     d  oldDIV                12     14a                                           DIV  3
     d  oldQTE                40     40a                                           QTE  1

     dNewKey           ds                  inz                                   New Key
     d  newCO#                 1      3a                                           CO#  3
     d  newFND                 4      6a                                           FND  3
     d  newEMP#                7     15a                                           EMP# 9
     d  newDIV                16     20a                                           DIV  5
     d  newQTE                40     40a                                           QTE  1

     c                   eval      *in71=*on
     c                   eval      xxDATE=*date
     c                   time                    xxTIME
     c                   read      sdcmp                                  91
     c                   dow       *in91=*off                                    >---+
     c                   eval      inCTCM=inCTCM+1                                   :
     c                   if        cmPNAM='W0612R'                               >-+ :
     c                   eval      upCTCM=upCTCM+1                                 : :
     c                   eval      OldKey=cmKEY                                    : :
     c                   exsr      $SwitchIt                                       : :
     c                   eval      cmKEY=NewKey                                    : :
     c                   eval      cmPNAM='W0050R'                                 : :
     c                   update    pcm01                                           : :
     c                   endif                                                   <-+ :
     c                   read      sdcmp                                  91         :
     c                   enddo                                                   <---+

     c                   read      smarp                                  91
     c                   dow       *in91=*off                                    >---+
     c                   eval      inCTAR=inCTAR+1                                   :
     c                   if        arPNAM='W0612R'                               >-+ :
     c                   eval      upCTAR=upCTAR+1                                 : :
     c                   eval      OldKey=arKEY                                    : :
     c                   exsr      $SwitchIt                                       : :
     c                   eval      arKEY=NewKey                                    : :
     c                   eval      arPNAM='W0050R'                                 : :
     c                   update    par01                                           : :
     c                   endif                                                   <-+ :
     c                   read      smarp                                  91         :
     c                   enddo                                                   <---+

     c                   read      smhdp                                  91
     c                   dow       *in91=*off                                    >---+
     c                   eval      inCTHD=inCTHD+1                                   :
     c                   if        hdPNAM='W0612R'                               >-+ :
     c                   eval      upCTHD=upCTHD+1                                 : :
     c                   eval      OldKey=hdKEY                                    : :
     c                   exsr      $SwitchIt                                       : :
     c                   eval      hdKEY=NewKey                                    : :
     c                   eval      hdPNAM='W0050R'                                 : :
     c                   update    phd01                                           : :
     c                   endif                                                   <-+ :
     c                   read      smhdp                                  91         :
     c                   enddo                                                   <---+

     c                   read      smh1p                                  LR
     c                   dow       *inLR=*off                                    >---+
     c                   eval      inCTH1=inCTH1+1                                   :
     c                   if        h1PNAM='W0612R'                               >-+ :
     c                   eval      upCTH1=upCTH1+1                                 : :
     c                   eval      OldKey=h1KEY                                    : :
     c                   exsr      $SwitchIt                                       : :
     c                   eval      h1KEY=NewKey                                    : :
     c                   eval      h1PNAM='W0050R'                                 : :
     c                   update    ph101                                           : :
     c                   endif                                                   <-+ :
     c                   read      smh1p                                  LR         :
     c                   enddo                                                   <---+

     c                   if        *in71=*on                                     >-+
     c                   except    ##Header                                        :
     c                   eval      *in71=*off                                      :
     c                   endif                                                   <-+

     c                   except    ##Totals

     csr   $SwitchIt     begsr                                                   >-+
     csr                 eval      NewKey=*blanks                                  :
     csr                 eval      newCO#=oldCO#                                   :
     csr                 eval      newFND=oldFND                                   :
     csr                 eval      newEMP#='0000'+oldEMP#                          :
     csr                 eval      newDIV='00'+oldDIV                              :
     csr                 eval      newQTE=oldQTE                                   :
     csr                 endsr                                                   <-+

     oqsysprt   e            ##Header       2 02
     o                       xxDATE              10 '  /  /    '
     o                       xxTIME              20 '  :  :  '
     o                                           75 'Adjust Program Key'
     o                                          128 'Page'
     o                       PAGE          4    132

     o          e            ##Totals       1
     o                                            6 'SDCMP:'
     o          e            ##Totals       1
     o                                           10 'Input'
     o                       inCTCM        3     24
     o          e            ##Totals       2    10
     o                                           10 'Updated'
     o                       upCTCM        3     24

     o          e            ##Totals       1
     o                                            6 'SMARP:'
     o          e            ##Totals       1
     o                                           10 'Input'
     o                       inCTAR        3     24
     o          e            ##Totals       2    10
     o                                           10 'Updated'
     o                       upCTAR        3     24

     o          e            ##Totals       1
     o                                            6 'SMHDP:'
     o          e            ##Totals       1
     o                                           10 'Input'
     o                       inCTHD        3     24
     o          e            ##Totals       2    10
     o                                           10 'Updated'
     o                       upCTHD        3     24

     o          e            ##Totals       1
     o                                            6 'SMH1P:'
     o          e            ##Totals       1
     o                                           10 'Input'
     o                       inCTH1        3     24
     o          e            ##Totals       2    10
     o                                           10 'Updated'
     o                       upCTH1        3     24
