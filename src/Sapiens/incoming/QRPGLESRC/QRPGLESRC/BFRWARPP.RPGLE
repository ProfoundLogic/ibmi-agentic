     hdatedit(*ymd) datfmt(*ymd)

     fWARPP     if   f  358        disk                                          Input file
     fAFTWARPPO o    e             printer oflind(*in71)                         Query report

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

      ** REQUEST PAYMENT FILE
      *************************************************
     iWARPP     ns
     i                             P    1    2 0RPCO#                            COMPANY NUMBER
     i                             P    3    4 0RPFND                            FUND NUMBER
     i                             P    5    6 0RPFYR                            FUND CENTURY AN
     i                             P    7   10 0RPCASE                           CASE NUMBER
     i                             P   11   13 0RPSEQ#                           SEQUENCE NUMBER
     i                                 14   19 0RPTRDT                           TRANSACTION DAT
     i                             P   20   24 0RPFEIN                           PAYEE FEDERAL I
     i                             P   25   26 0RPMOD                            MODIFIER
     i                             P   27   31 2RPAMT                            AMOUNT
     i                                 32   33  RPBR2                            PAYMENT VALUE 1
     i                                 34   35  RPBR7                            PAYMENT VALUE 2
     i                                 36   37  RPNCCI                           NCCI CODE
     i                                 38   43 0RPFRDT                           FROM DATE
     i                                 44   49 0RPTODT                           TO DATE
     i                             P   50   51 0RPWKS                            NUMBER OF WEEKS
     i                             P   52   52 0RPDAYS                           NUMBER OF DAYS
     i                                 53   54  RPTRCD                           TRANSACTION COD
     i                                 55   55  RPTYPE                           RECORD TYPE
     i                                 56   56  RPRETP                           RESERVE TYPE
     i                                 57   58  RPRECD                           RESERVE CODE
     i                                 59   88  RPRMDS                           REMARKS
     i                                 89   90  RPRMK1                           REMARK CODE 1
     i                                 91   92  RPRMK2                           REMARK CODE 2
     i                                 93   94  RPRMK3                           REMARK CODE 3
     i                                 95  124  RPDESC                           DESCRIPTION
     i                                125  126  RPCKCN                           CHECK CONTROL
     i                                127  166  RPPNAM                           NAME
     i                                167  206  RPADR1                           ADDRESS LINE 1
     i                                207  246  RPADR2                           ADDRESS LINE 2
     i                                247  271  RPCTY                            CITY
     i                                272  273  RPST                             STATE
     i                             P  274  278 0RPZIP                            ZIP CODE
     i                                279  279  RPSTAT                           STATUS
     i                                280  289  RPUSER                           USER
     i                                290  299  RPPGM                            PROGRAM
     i                                300  305 0RPTIME                           TIME
     i                                306  311 0RPDT                             DATE MAINTAINED
     i                             P  312  316 0RPBUL1                           BATCH UPDATE LO
     i                             P  317  320 0RPCHK#                           CHECK NUMBER
     i                             P  321  325 0RPEOB#                           MED FEE SCHED E
     i                                326  331 0RPRCVD                           DATE RECEIVED
     i                                332  337 0RPDUE                            DUE DATE
     i                                338  339  RPSTS                            STATUS CODE
     i                                340  340  RPSORT                           SORT SEQUENCE
     i                                341  341  RPSENT                           DIARY SENT FLAG
     i                             P  342  344 0RPDSEQ                           DIARY SEQUENCE
     i                                345  345  RPPGTP                           GROUP TYPE
     i                                346  355  RPRQST                           INITIAL REQUEST
     i                                356  357  RPLTCD                           PMT LATE CODE
     i                                358  358  RPATYP                           BANK ACCT TYPE

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      warpp                                  LR
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
     csr                 eval      rpCO#=*loval                                    :
     csr                 eval      rpFND=*loval                                    :
     csr                 eval      rpFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxAMT=xxAMT+rpAMT                                 :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=rpCO#                                     : :
     csr                 eval      xxFND=rpFND                                     : :
     csr                 eval      xxFYR=rpFYR                                     : :
     csr                 eval      xxTRCD=rpTRCD                                   : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>rpCO# or xxFND<>rpFND                  >-+ :
     c                             or xxFYR<>rpFYR or xxTRCD<>rpTRCD               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=rpCO#                                     : :
     csr                 eval      xxFND=rpFND                                     : :
     csr                 eval      xxFYR=rpFYR                                     : :
     csr                 eval      xxTRCD=rpTRCD                                   : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxAMT1=xxAMT1+rpAMT                               :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxAMT2=xxAMT2+rpAMT                               :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 eval      xxAMT3=xxAMT3+rpAMT                               :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>rpCO# or xxFND<>rpFND or xxFYR<>rpFYR  >-+   :
     csr                 eval      *in61=*on                                       :   :
     csr                 eval      xxPRINT=*zero                                   :   :
     csr                 endif                                                   <-+   :
     csr                 write     PRTBK1                                              :
     csr                 eval      xxPRINT=xxPRINT+1                                   :
     csr                 eval      xxAMT1=*zero                                        :
     csr                 eval      xxCOUNT1=*zero                                      :
     csr                 if        *in61=*on                                     >---+ :
     csr                 write     PRTBK2                                            : :
     csr                 eval      *in61=*off                                        : :
     csr                 eval      xxAMT2=*zero                                      : :
     csr                 eval      xxCOUNT2=*zero                                    : :
     csr                 if        xxCO#<>rpCO# or xxFND<>rpFND                  >-+ : :
     csr                 write     PRTBK3                                          : : :
     csr                 eval      xxAMT3=*zero                                    : : :
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
