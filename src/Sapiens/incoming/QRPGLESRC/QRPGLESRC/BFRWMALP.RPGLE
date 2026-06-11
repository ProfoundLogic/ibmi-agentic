     hdatedit(*ymd) datfmt(*ymd)

     fWMALP     if   f  875        disk                                          Input file
     fAFTWMALPO o    e             printer oflind(*in71)                         Query report

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

      ** ADDRESS LOCATION FILE
      *************************************************
     iWMALP     ns
     i                             P    1    2 0ALCO#                            COMPANY NUMBER
     i                             P    3    4 0ALFND                            FUND NUMBER
     i                             P    5    7 0ALEMP#                           EMPLOYER NUMBER
     i                             P    8    9 0ALDIV                            DIVISION
     i                                 10   10  ALELTP                           ENTITY/LOCATION
     i                                 11   20  ALELID                           LOCATION ID
     i                                 21   26 0ALDLDT                           DELETION DATE
     i                                 27   56  ALDLRS                           DELETION REASON
     i                                 57   62 0ALEFDT                           EFFECTIVE DATE
     i                                 63  102  ALNAME                           EMPLOYER NAME
     i                                103  142  ALDBA                            DBA NAME
     i                                143  182  ALMAD1                           MAILING ADDRESS
     i                                183  222  ALMAD2                           MAILING ADDRESS
     i                                223  247  ALMCTY                           MAILING CITY
     i                                248  249  ALMST                            MAILING STATE
     i                             P  250  254 0ALMZIP                           MAILING ZIP COD
     i                                255  294  ALPAD1                           PHYSICAL ADDRES
     i                                295  334  ALPAD2                           PHYSICAL ADDRES
     i                                335  359  ALPCTY                           PHYSICAL CITY
     i                                360  361  ALPST                            PHYSICAL STATE
     i                             P  362  366 0ALPZIP                           PHYSICAL ZIP CO
     i                                367  367  ALTPOB                           TYPE OF BUSINES
     i                             P  368  372 0ALFEIN                           EMPLOYER FEDERA
     i                                373  402  ALOLN1                           OFFICERS LAST N
     i                                403  432  ALOLN2                           OFFICERS LAST N
     i                                433  462  ALOLN3                           OFFICERS LAST N
     i                                463  492  ALOLN4                           OFFICERS LAST N
     i                                493  522  ALOLN5                           OFFICERS LAST N
     i                                523  552  ALOLN6                           OFFICERS LAST N
     i                                553  582  ALOLN7                           OFFICERS LAST N
     i                                583  597  ALOFN1                           OFFICERS FIRST
     i                                598  612  ALOFN2                           OFFICERS FIRST
     i                                613  627  ALOFN3                           OFFICERS FIRST
     i                                628  642  ALOFN4                           OFFICERS FIRST
     i                                643  657  ALOFN5                           OFFICERS FIRST
     i                                658  672  ALOFN6                           OFFICERS FIRST
     i                                673  687  ALOFN7                           OFFICERS FIRST
     i                                688  688  ALONC1                           OWN. EXEMPT.1 Y
     i                                689  689  ALONC2                           OWN. EXEMPT.2 Y
     i                                690  690  ALONC3                           OWN. EXEMPT.3 Y
     i                                691  691  ALONC4                           OWN. EXEMPT.4 Y
     i                                692  692  ALONC5                           OWN. EXEMPT.5 Y
     i                                693  693  ALONC6                           OWN. EXEMPT.6 Y
     i                                694  694  ALONC7                           OWN. EXEMPT.7 Y
     i                                695  700 0ALED01                           OWN. EXEMPT.1 D
     i                                701  706 0ALED02                           OWN. EXEMPT.2 D
     i                                707  712 0ALED03                           OWN. EXEMPT.3 D
     i                                713  718 0ALED04                           OWN. EXEMPT.4 D
     i                                719  724 0ALED05                           OWN. EXEMPT.5 D
     i                                725  730 0ALED06                           OWN. EXEMPT.6 D
     i                                731  736 0ALED07                           OWN. EXEMPT.7 D
     i                             P  737  739 2ALOFP1                           OFFICERS PERCEN
     i                             P  740  742 2ALOFP2                           OFFICERS PERCEN
     i                             P  743  745 2ALOFP3                           OFFICERS PERCEN
     i                             P  746  748 2ALOFP4                           OFFICERS PERCEN
     i                             P  749  751 2ALOFP5                           OFFICERS PERCEN
     i                             P  752  754 2ALOFP6                           OFFICERS PERCEN
     i                             P  755  757 2ALOFP7                           OFFICERS PERCEN
     i                                758  766  ALSS#1                           SOC. SEC. # FOR
     i                                767  775  ALSS#2                           SOC. SEC. # FOR
     i                                776  784  ALSS#3                           SOC. SEC. # FOR
     i                                785  793  ALSS#4                           SOC. SEC. # FOR
     i                                794  802  ALSS#5                           SOC. SEC. # FOR
     i                                803  811  ALSS#6                           SOC. SEC. # FOR
     i                                812  820  ALSS#7                           SOC. SEC. # FOR
     i                                821  825  ALRMCD                           REMARK CODE
     i                                826  826  ALSTAT                           STATUS
     i                                827  836  ALUSER                           USER
     i                                837  846  ALPGM                            PROGRAM
     i                                847  852 0ALTIME                           TIME
     i                                853  858 0ALDT                             DATE MAINTAINED
     i                                859  868  ALIUSR                           RECORD CREATED
     i                                869  874 0ALIDT                            DATE RECORD CRE
     i                                875  875  ALNCCI                           REPORTED TO NCC

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wmalp                                  LR
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
     csr                 eval      alCO#=*loval                                    :
     csr                 eval      alFND=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=alCO#                                     : :
     csr                 eval      xxFND=alFND                                     : :
     csr                 eval      xxEMP#=alEMP#                                   : :
     csr                 eval      xxDIV=alDIV                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>alCO# or xxFND<>alFND                  >-+ :
     c                             or xxEMP#<>alEMP# or xxDIV<>alDIV               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=alCO#                                     : :
     csr                 eval      xxFND=alFND                                     : :
     csr                 eval      xxEMP#=alEMP#                                   : :
     csr                 eval      xxDIV=alDIV                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >---+
     csr                 if        xxCO#<>alCO# or xxFND<>alFND                  >-+ :
     csr                 eval      *in61=*on                                       : :
     csr                 eval      xxPRINT=*zero                                   : :
     csr                 endif                                                   <-+ :
     csr                 write     PRTBK1                                            :
     csr                 eval      xxPRINT=xxPRINT+1                                 :
     csr                 eval      xxCOUNT1=*zero                                    :
     csr                 if        *in61=*on                                     >-+ :
     csr                 write     PRTBK2                                          : :
     csr                 eval      *in61=*off                                      : :
     csr                 eval      xxCOUNT2=*zero                                  : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

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
