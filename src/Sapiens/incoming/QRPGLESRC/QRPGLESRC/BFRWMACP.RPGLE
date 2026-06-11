     hdatedit(*ymd) datfmt(*ymd)

     fWMACP     if   f  117        disk                                          Input file
     fAFTWMACPO o    e             printer oflind(*in71)                         Query report

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

      ** AGENT COMMISSIONS FILE
      *************************************************
     iWMACP     ns
     i                             P    1    2 0ACCO#                            COMPANY NUMBER
     i                             P    3    4 0ACFND                            FUND NUMBER
     i                             P    5    6 0ACFYR                            FUND CENTURY AN
     i                             P    7    9 0ACEMP#                           EMPLOYER NUMBER
     i                             P   10   11 0ACDIV                            DIVISION
     i                             P   12   14 0ACAGT#                           AGENT # ASSIGNE
     i                             P   15   17 0ACPROD                           PRODUCER NUMBER
     i                                 18   23 0ACPODT                           POSTING DATE
     i                                 24   29 0ACTRDT                           TRANSACTION DAT
     i                             P   30   35 2ACTOCL                           TOTAL COLLECTIO
     i                             P   36   38 2ACRATE                           RATE
     i                             P   39   42 2ACAMT                            AMOUNT
     i                             P   43   46 0ACCHCK                           CHECK #
     i                                 47   48  ACCOCD                           COMMISSION CODE
     i                                 49   49  ACCOTP                           COMMISSION TYPE
     i                                 50   79  ACDESC                           DESCRIPTION
     i                                 80   80  ACSTAT                           STATUS
     i                                 81   90  ACUSER                           USER
     i                                 91  100  ACPGM                            PROGRAM
     i                                101  106 0ACTIME                           TIME
     i                                107  112 0ACDT                             DATE MAINTAINED
     i                             P  113  117 0ACBUL2                           BATCH UPDATE LO

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wmacp                                  LR
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
     csr                 eval      acCO#=*loval                                    :
     csr                 eval      acFND=*loval                                    :
     csr                 eval      acFYR=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxAMT=xxAMT+acAMT                                 :
     csr                 eval      xxTOCL=xxTOCL+acTOCL                              :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=acCO#                                     : :
     csr                 eval      xxFND=acFND                                     : :
     csr                 eval      xxFYR=acFYR                                     : :
     csr                 eval      xxCOCD=acCOCD                                   : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>acCO# or xxFND<>acFND                  >-+ :
     c                             or xxFYR<>acFYR or xxCOCD<>acCOCD               : :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=acCO#                                     : :
     csr                 eval      xxFND=acFND                                     : :
     csr                 eval      xxFYR=acFYR                                     : :
     csr                 eval      xxCOCD=acCOCD                                   : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxAMT1=xxAMT1+acAMT                               :
     csr                 eval      xxTOCL1=xxTOCL1+acTOCL                            :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxAMT2=xxAMT2+acAMT                               :
     csr                 eval      xxTOCL2=xxTOCL2+acTOCL                            :
     csr                 eval      xxCOUNT3=xxCOUNT3+1                               :
     csr                 eval      xxAMT3=xxAMT3+acAMT                               :
     csr                 eval      xxTOCL3=xxTOCL3+acTOCL                            :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >-----+
     csr                 if        xxCO#<>acCO# or xxFND<>acFND or xxFYR<>acFYR  >-+   :
     csr                 eval      *in61=*on                                       :   :
     csr                 eval      xxPRINT=*zero                                   :   :
     csr                 endif                                                   <-+   :
     csr                 write     PRTBK1                                              :
     csr                 eval      xxPRINT=xxPRINT+1                                   :
     csr                 eval      xxAMT1=*zero                                        :
     csr                 eval      xxTOCL1=*zero                                       :
     csr                 eval      xxCOUNT1=*zero                                      :
     csr                 if        *in61=*on                                     >---+ :
     csr                 write     PRTBK2                                            : :
     csr                 eval      *in61=*off                                        : :
     csr                 eval      xxAMT2=*zero                                      : :
     csr                 eval      xxTOCL2=*zero                                     : :
     csr                 eval      xxCOUNT2=*zero                                    : :
     csr                 if        xxCO#<>acCO# or xxFND<>acFND                  >-+ : :
     csr                 write     PRTBK3                                          : : :
     csr                 eval      xxAMT3=*zero                                    : : :
     csr                 eval      xxTOCL3=*zero                                   : : :
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
