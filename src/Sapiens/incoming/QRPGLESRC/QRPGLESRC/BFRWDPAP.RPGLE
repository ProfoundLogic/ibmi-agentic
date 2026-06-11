     hdatedit(*ymd) datfmt(*ymd)

     fWDPAP     if   f  179        disk                                          Input file
     fAFTWDPAPO o    e             printer oflind(*in71)                         Query report

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

      ** CLAIMS CHECK PAYMENT FILE
      *************************************************
     iWDPAP     ns
     i                             P    1    2 0PACO#                            COMPANY NUMBER
     i                             P    3    4 0PAFND                            FUND NUMBER
     i                             P    5    6 0PAFYR                            FUND CENTURY AN
     i                             P    7   10 0PACASE                           CASE NUMBER
     i                             P   11   13 0PASEQ#                           SEQUENCE NUMBER
     i                             P   14   17 0PACHK#                           CHECK NUMBER
     i                             P   18   20 0PABASQ                           BASIS SEQUENCE
     i                                 21   26 0PATRDT                           TRANSACTION DAT
     i                                 27   32 0PAPODT                           POSTING DATE
     i                             P   33   37 0PAFEIN                           PAYEE FEDERAL I
     i                             P   38   39 0PAMOD                            PAYEE MODIFIER
     i                             P   40   44 2PAAMT                            AMOUNT
     i                                 45   46  PABR2                            PAYMENT VALUE 1
     i                                 47   48  PABR7                            PAYMENT VALUE 2
     i                                 49   54 0PAFRDT                           FROM DATE
     i                                 55   60 0PATODT                           TO DATE
     i                             P   61   62 0PAWKS                            NUMBER OF WEEKS
     i                             P   63   63 0PADAYS                           NUMBER OF DAYS
     i                                 64   65  PATRCD                           TRANSACTION COD
     i                                 66   66  PATYPE                           RECORD TYPE
     i                                 67   68  PARECD                           RESERVE CODE
     i                                 69   69  PARETP                           RESERVE TYPE
     i                                 70   99  PADESC                           DESCRIPTION
     i                                100  100  PAADST                           ADJUSTMENT STAT
     i                                101  101  PASTAT                           STATUS
     i                                102  111  PAUSER                           USER
     i                                112  121  PAPGM                            PROGRAM
     i                                122  127 0PATIME                           TIME
     i                                128  133 0PADT                             DATE MAINTAINED
     i                             P  134  138 0PABUL1                           BATCH UPDATE LO
     i                                139  144 0PARCVD                           DATE RECEIVED
     i                                145  150 0PADUE                            DUE DATE
     i                                151  151  PAPGTP                           CHECK TYPE GROU
     i                             P  152  156 0PAEOB#                           EOMB#
     i                                157  158  PALTCD                           PMT LATE CODE
     i                                159  159  PAATYP                           BANK ACCT TYPE
     i                                160  179  PAACCT                           BANK ACCT NUMBE

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wdpap                                  LR
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
     csr                 eval      paCO#=*loval                                    :
     csr                 eval      paFND=*loval                                    :
     csr                 eval      paFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxAMT=xxAMT+paAMT                                 :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=paCO#                                     : :
     csr                 eval      xxFND=paFND                                     : :
     csr                 eval      xxFYR=paFYR                                     : :
     csr                 eval      xxTYPE=paTYPE                                   : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>paCO# or xxFND<>paFND                  >-+ :
     c                             or xxFYR<>paFYR or xxTYPE<>paTYPE               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=paCO#                                     : :
     csr                 eval      xxFND=paFND                                     : :
     csr                 eval      xxFYR=paFYR                                     : :
     csr                 eval      xxTYPE=paTYPE                                   : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxAMT1=xxAMT1+paAMT                               :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxAMT2=xxAMT2+paAMT                               :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 eval      xxAMT3=xxAMT3+paAMT                               :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>paCO# or xxFND<>paFND or xxFYR<>paFYR  >-+   :
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
     csr                 if        xxCO#<>paCO# or xxFND<>paFND                  >-+ : :
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
