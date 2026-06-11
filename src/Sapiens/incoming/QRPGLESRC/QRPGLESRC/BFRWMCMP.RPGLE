     hdatedit(*ymd) datfmt(*ymd)

     fWMCMP     if   f  562        disk                                          Input file
     fAFTWMCMPO o    e             printer oflind(*in71)                         Query report

      ** Constants
      *************************************************
     dcnTITLE          c                   const('Query Conversion for')         Title
     dcnTYPE           c                   const('Old File')                     File (new/old)

      ** Initialization work area
      *************************************************
     dWork_Inz         ds                  inz                                   Initialization
     d  xxCCYYMMDD                    8s 0                                         Date is CCYYMMDD
     d    xxCCYY                      4a   overlay(xxCCYYMMDD:1)                     Century, year
     d    xxMM                        2a   overlay(xxCCYYMMDD:5)                     Month
     d    xxDD                        2a   overlay(xxCCYYMMDD:7)                     Day
     d  xxEDATE                      10a                                           Edited date
     d  xxHRMNSC                      6s 0                                         Time is HHMMSS
     d    xxHR                        2a   overlay(xxHRMNSC:1)                       Hour
     d    xxMN                        2a   overlay(xxHRMNSC:3)                       Minute
     d    xxSC                        2a   overlay(xxHRMNSC:5)                       Second
     d  xxETIME                       8a                                           Edited time

      ** Work/Storage area
      *************************************************
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxPRINT                       9p 0                                         Print counter

      ** CLAIM FILE
      *************************************************
     iWMCMP     ns
     i                             P    1    2 0CMCO#                            COMPANY NUMBER
     i                             P    3    4 0CMFND                            FUND NUMBER
     i                             P    5    6 0CMFYR                            FUND CENTURY AN
     i                             P    7    9 0CMEMP#                           EMPLOYER NUMBER
     i                             P   10   11 0CMDIV                            DIVISION
     i                             P   12   15 0CMCASE                           CASE NUMBER
     i                                 16   24  CMSS#                            SOCIAL SECURITY
     i                             P   25   27 0CMCLAS                           CLASS CODE
     i                             P   28   31 2CMAWW                            AVERAGE WEEKLY
     i                             P   32   32 0CMDAYS                           NUMBER OF DAYS
     i                                 33   38 0CMACDT                           ACCIDENT DATE
     i                                 39   44 0CMACTM                           ACCIDENT TIME
     i                                 45   50 0CMDTRP                           DATE REPORTED
     i                                 51   56 0CMDTLW                           DATE LAST WORKE
     i                                 57  116  CMACDS                           EMPLOEYRS ACC D
     i                                117  117  CMLTFL                           LOST TIME FLAG
     i                                118  119  CMNCCI                           NCCI CODE
     i                                120  121  CMNATR                           NATURE CODE
     i                                122  123  CMPART                           PART CODE
     i                                124  125  CMCAUS                           CAUSE CODE
     i                                126  128  CMSUPR                           SUPERVISOR
     i                                129  131  CMCLEX                           CLAIMS EXAMINER
     i                                132  134  CMFEAD                           FIELD ADJUSTOR
     i                                135  135  CMMCON                           METHOD OF CONTA
     i                                136  141 0CMDTIC                           DATE INITIAL CO
     i                                142  143  CMRT01                           RESERVE TYPE CO
     i                                144  145  CMRT02                           RESERVE TYPE CO
     i                                146  147  CMRT03                           RESERVE TYPE CO
     i                                148  149  CMRT04                           RESERVE TYPE CO
     i                                150  151  CMRT05                           RESERVE TYPE CO
     i                                152  153  CMRT06                           RESERVE TYPE CO
     i                                154  155  CMRT07                           RESERVE TYPE CO
     i                                156  157  CMRT08                           RESERVE TYPE CO
     i                                158  159  CMRT09                           RESERVE TYPE CO
     i                                160  161  CMRT10                           RESERVE TYPE CO
     i                             P  162  166 2CMRR01                           REMAINING RESER
     i                             P  167  171 2CMRR02                           REMAINING RESER
     i                             P  172  176 2CMRR03                           REMAINING RESER
     i                             P  177  181 2CMRR04                           REMAINING RESER
     i                             P  182  186 2CMRR05                           REMAINING RESER
     i                             P  187  191 2CMRR06                           REMAINING RESER
     i                             P  192  196 2CMRR07                           REMAINING RESER
     i                             P  197  201 2CMRR08                           REMAINING RESER
     i                             P  202  206 2CMRR09                           REMAINING RESER
     i                             P  207  211 2CMRR10                           REMAINING RESER
     i                             P  212  216 2CMAR01                           PMT AGAINST RES
     i                             P  217  221 2CMAR02                           PMT AGAINST RES
     i                             P  222  226 2CMAR03                           PMT AGAINST RES
     i                             P  227  231 2CMAR04                           PMT AGAINST RES
     i                             P  232  236 2CMAR05                           PMT AGAINST RES
     i                             P  237  241 2CMAR06                           PMT AGAINST RES
     i                             P  242  246 2CMAR07                           PMT AGAINST RES
     i                             P  247  251 2CMAR08                           PMT AGAINST RES
     i                             P  252  256 2CMAR09                           PMT AGAINST RES
     i                             P  257  261 2CMAR10                           PMT AGAINST RES
     i                                262  271  CMCTG1                           CATEGORY 1
     i                                272  281  CMCTG2                           CATEGORY 2
     i                             P  282  286 0CMDEFA                           DEFENSE ATTORNE
     i                             P  287  288 0CMDEFM                           DEFENSE ATTORNE
     i                             P  289  293 0CMPLAA                           PLAINTIFF ATTY
     i                             P  294  295 0CMPLAM                           PLAINTIFF ATTY
     i                                296  301 0CMDTRE                           DATE REHAB ASSI
     i                                302  302  CMSTAT                           STATUS
     i                                303  312  CMUSER                           USER
     i                                313  322  CMPGM                            PROGRAM
     i                                323  328 0CMTIME                           TIME
     i                                329  334 0CMDT                             DATE MAINTAINED
     i                             P  335  337 0CMPCTR                           PAYMENT COUNTER
     i                             P  338  342 0CMBUL1                           BATCH LOG NO. 1
     i                                343  348 0CMENTD                           DATE ENTERED
     i                                349  354 0CMRCVD                           DATE RECEIVED
     i                                355  355  CMPLFL                           INTAKE STATUS
     i                                356  358  CMUMAN                           UNIT MANAGER
     i                                359  359  CMFSAL                           FULL SALARY
     i                                360  365 0CMFSDT                           PAID THRU DATE
     i                                366  405  CMAADR                           PLACE OF ACCIDE
     i                                406  430  CMACTY                           ACCIDENT CITY
     i                                431  432  CMAST                            ACCIDENT STATE
     i                             P  433  435 0CMAZP1                           ACCIDENT ZIP CO
     i                             P  436  438 0CMAZP2                           ACCIDENT ZIP CO
     i                                439  444 0CMDOH                            DATE OF HIRE
     i                                445  445  CMEAGR                           EMPLOYER AGREE
     i                                446  448  CMSDFA                           SDF ADJUSTER
     i                                449  451  CMSDFR                           SDF FIELD REP
     i                                452  454  CMSADJ                           SDF/SUBRO ADJUS
     i                                455  457  CMESUP                           EXCESS SUPERVIS
     i                                458  458  CMCVFL                           CONTROVERT FLAG
     i                                459  464 0CMDDAT                           DESTRUCTION DAT
     i                             P  465  468 2CMPTS                            PERM TOTAL SUPP
     i                             P  469  472 2CMSSO                            SOCIAL SECURITY
     i                                473  473  CMSUBR                           SUBROGATION FLA
     i                                474  474  CMSDF                            SDF FLAG
     i                                475  475  CMMCO                            MANAGED CARE FL
     i                                476  481 0CMMMI                            FINAL MMI DATE
     i                                482  521  CMANAM                           PLACE OF ACC
     i                                522  531  CMEUSR                           ENTRY LOST TIME
     i                                532  537 0CMIDT                            INITIAL ENTRY D
     i                                538  547  CMIUSR                           INITIAL ENTRY U
     i                                548  553 0CMITM                            INITIAL ENTRY T
     i                                554  556  CMCMNU                           CM NURSE
     i                                557  562 0CMMCLB                           LABEL PRINT DT

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

     c     *like         define    cmRR01        tmRAM            +2
     c     *like         define    cmAR01        tmPTD            +2

      ** Mainline processing
      *************************************************
     c                   read      wmcmp                                  LR
     c                   if        *inLR=*on                                     >-+
     c                   exsr      $Finish                                         :
     c                   else                                                    >-+
     c                   exsr      $Process                                        :
     c                   endif                                                   <-+

      ** Program initialization routine
      *************************************************
     csr   *inzsr        begsr                                                   >-+
     csr                 move      *date         xxCCYYMMDD                        :
     csr                 time                    xxHRMNSC                          :
     csr                 eval      xxEDATE=xxMM+'/'+xxDD+'/'+xxCCYY                :
     csr                 eval      xxETIME=xxHR+':'+xxMN+':'+xxSC                  :
     csr                 eval      *in71=*on                                       :
     csr                 eval      ptPAGE=*zero                                    :
     csr                 eval      xxCOUNT=*zero                                   :
     csr                 eval      ptRDAT=xxEDATE                                  :
     csr                 eval      ptRTIM=xxETIME                                  :
     csr                 eval      ptTITL=cnTITLE+' '+cnTYPE+' '+pmFILE            :
     csr   ' '           checkr    ptTITL        IX                                :
     csr                 eval      IX=(%size(ptTITL)-IX)/2                         :
     csr   ' '           cat       ptTITL:IX     ptTITL                            :
     csr                 eval      ptQFIL=%trim('*LIBL  ')+'/'+pmFILE              :
     csr                 endsr                                                   <-+

      ** Finish up routine
      *************************************************
     csr   $Finish       begsr                                                   >-+
     csr                 eval      cmCO#=*loval                                    :
     csr                 eval      cmFND=*loval                                    :
     csr                 eval      cmFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      tmRAM=cmRR01+cmRR02+cmRR03+cmRR04+cmRR05+         :
     c                             cmRR06+cmRR07+cmRR08+cmRR09+cmRR10                :
     csr                 eval      tmPTD=cmAR01+cmAR02+cmAR03+cmAR04+cmAR05+         :
     c                             cmAR06+cmAR07+cmAR08+cmAR09+cmAR10                :
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxRAM=xxRAM+tmRAM                                 :
     csr                 eval      xxPTD=xxPTD+tmPTD                                 :
     csr                 eval      xxINC=xxINC+tmRAM+tmPTD                           :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=cmCO#                                     : :
     csr                 eval      xxFND=cmFND                                     : :
     csr                 eval      xxFYR=cmFYR                                     : :
     csr                 eval      xxSTAT=cmSTAT                                   : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>cmCO# or xxFND<>cmFND                  >-+ :
     c                             or xxFYR<>cmFYR or xxSTAT<>cmSTAT               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=cmCO#                                     : :
     csr                 eval      xxFND=cmFND                                     : :
     csr                 eval      xxFYR=cmFYR                                     : :
     csr                 eval      xxSTAT=cmSTAT                                   : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxRAM1=xxRAM1+tmRAM                               :
     csr                 eval      xxPTD1=xxPTD1+tmPTD                               :
     csr                 eval      xxINC1=xxINC1+tmRAM+tmPTD                         :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxRAM2=xxRAM2+tmRAM                               :
     csr                 eval      xxPTD2=xxPTD2+tmPTD                               :
     csr                 eval      xxINC2=xxINC2+tmRAM+tmPTD                         :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 eval      xxRAM3=xxRAM3+tmRAM                               :
     csr                 eval      xxPTD3=xxPTD3+tmPTD                               :
     csr                 eval      xxINC3=xxINC3+tmRAM+tmPTD                         :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>cmCO# or xxFND<>cmFND or xxFYR<>cmFYR  >-+   :
     csr                 eval      *in61=*on                                       :   :
     csr                 eval      xxPRINT=*zero                                   :   :
     csr                 endif                                                   <-+   :
     csr                 write     PRTBK1                                              :
     csr                 eval      xxPRINT=xxPRINT+1                                   :
     csr                 eval      xxRAM1=*zero                                        :
     csr                 eval      xxPTD1=*zero                                        :
     csr                 eval      xxINC1=*zero                                        :
     csr                 eval      xxCOUNT1=*zero                                      :
     csr                 if        *in61=*on                                     >---+ :
     csr                 write     PRTBK2                                            : :
     csr                 eval      *in61=*off                                        : :
     csr                 eval      xxRAM2=*zero                                      : :
     csr                 eval      xxPTD2=*zero                                      : :
     csr                 eval      xxINC2=*zero                                      : :
     csr                 eval      xxCOUNT2=*zero                                    : :
     csr                 if        xxCO#<>cmCO# or xxFND<>cmFND                  >-+ : :
     csr                 write     PRTBK3                                          : : :
     csr                 eval      xxRAM3=*zero                                    : : :
     csr                 eval      xxPTD3=*zero                                    : : :
     csr                 eval      xxINC3=*zero                                    : : :
     csr                 eval      xxCOUNT3=*zero                                  : : :
     csr                 endif                                                   <-+ : :
     csr                 write     PRTBLK                                            : :
     csr                 endif                                                   <---+ :
     csr                 endsr                                                   <-----+

      ** Write a print header
      *************************************************
     csr   $Write_Hd     begsr                                                   >---+
     csr                 if        *in71=*on                                     >-+ :
     csr                 eval      *in71=*off                                      : :
     csr                 eval      ptPAGE=ptPAGE+1                                 : :
     csr                 write     PRTHDR                                          : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      ** Write total line
      *************************************************
     csr   $Write_Tl     begsr                                                   >-+
     csr                 write     PRTTOT                                          :
     csr                 endsr                                                   <-+
