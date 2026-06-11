     hdatedit(*ymd) datfmt(*ymd)

     fWDXHP     if   f   83        disk                                          Input file
     fAFTWDXHPO o    e             printer oflind(*in71)                         Query report

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

      ** CANCELLATION HISTORY FILE
      *************************************************
     iWDXHP     ns
     i                             P    1    2 0XHCO#                            COMPANY NUMBER
     i                             P    3    4 0XHFND                            FUND NUMBER
     i                             P    5    6 0XHFYR                            FUND CENTURY AN
     i                             P    7    9 0XHEMP#                           EMPLOYER NUMBER
     i                             P   10   11 0XHDIV                            DIVISION
     i                             P   12   14 0XHREQ#                           SEQUENCE NUMBER
     i                                 15   20 0XHCXDT                           CANCEL DATE
     i                                 21   26 0XHCDAT                           DATE CANCELLATI
     i                             P   27   29 0XHAGT#                           AGENT NUMBER
     i                                 30   31  XHRSN                            CANCEL REASON
     i                                 32   37 0XHREDT                           REINSTATEMENT D
     i                                 38   43 0XHRDAT                           DATE REINSTATEM
     i                                 44   49 0XHTRDT                           CURRENT SYSTEM'
     i                                 50   50  XHRVFL                           REVISED FLAG
     i                                 51   51  XHSTAT                           STATUS
     i                                 52   61  XHUSER                           USER
     i                                 62   71  XHPGM                            PROGRAM
     i                                 72   77 0XHTIME                           TIME
     i                                 78   83 0XHDT                             DATE MAINTAINED

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wdxhp                                  LR
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
     csr                 eval      xhCO#=*loval                                    :
     csr                 eval      xhFND=*loval                                    :
     csr                 eval      xhFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=xhCO#                                     : :
     csr                 eval      xxFND=xhFND                                     : :
     csr                 eval      xxFYR=xhFYR                                     : :
     csr                 eval      xxEMP#=xhEMP#                                   : :
     csr                 eval      xxDIV=xhDIV                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>xhCO# or xxFND<>xhFND                  >-+ :
     c                             or xxFYR<>xhFYR or xxEMP#<>xhEMP#               : :
     c                             or xxDIV<>xhDIV                                 : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=xhCO#                                     : :
     csr                 eval      xxFND=xhFND                                     : :
     csr                 eval      xxFYR=xhFYR                                     : :
     csr                 eval      xxEMP#=xhEMP#                                   : :
     csr                 eval      xxDIV=xhDIV                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>xhCO# or xxFND<>xhFND or xxFYR<>xhFYR  >-+   :
     csr                 eval      *in61=*on                                       :   :
     csr                 eval      xxPRINT=*zero                                   :   :
     csr                 endif                                                   <-+   :
     csr                 write     PRTBK1                                              :
     csr                 eval      xxPRINT=xxPRINT+1                                   :
     csr                 eval      xxCOUNT1=*zero                                      :
     csr                 if        *in61=*on                                     >---+ :
     csr                 write     PRTBK2                                            : :
     csr                 eval      *in61=*off                                        : :
     csr                 eval      xxCOUNT2=*zero                                    : :
     csr                 if        xxCO#<>xhCO# or xxFND<>xhFND                  >-+ : :
     csr                 write     PRTBK3                                          : : :
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
