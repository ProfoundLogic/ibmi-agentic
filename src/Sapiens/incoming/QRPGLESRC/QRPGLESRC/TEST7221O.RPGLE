     fW7221O    O    E             PRINTER
     d xxpyr           s              3  0 inz(106)
     d xxemn1          s             50    inz('XXENM1XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXX')
     d xxemn2          s             50    inz('XXENM2XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXX')
     d xxemn3          s             50    inz('XXENM3XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXX')
     d xxemn4          s             50    inz('XXENM4XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXX')
     d xxemn5          s             50    inz('XXENM5XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXX')
     d xxemn6          s             50    inz('XXENM6XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXX')
     d xxemn7          s             50    inz('XXENM7XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXX')
     d xxcap#          s             36    inz('XXCAP#XXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                      XXX')
     d xpahdr          s             22    inz('XPAHDRXXXXXXXXXXXXXXXX')
     d xxanam          s             30    inz('XXANAMXXXXXXXXXXXXXXXXXXXXXXXX')
     d xxfnnm          s             60    inz('XXFFNMXXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXXXXXXXXXXXXXXXXXXXXXXXX')
     d xxpol#          s             15    inz('XXPOL#999999999')
     d emdiv           s              5  0 inz(99999)
     d xxrper          s             23    inz('99/99/9999 - 99/99/9999')
     d xxpdue          s             30    inz('Payment Due:  99/99/9999')
     d hdg1            s             27    inz('HDG1XXXXXXXXXXXXXXXXXXXXXXX')
     d hdg2            s             27    inz('HDG2XXXXXXXXXXXXXXXXXXXXXXX')
     d hdg3            s             27    inz('HDG3XXXXXXXXXXXXXXXXXXXXXXX')
     d hdg4            s             27    inz('HDG4XXXXXXXXXXXXXXXXXXXXXXX')
     d hdg5            s             27    inz('HDG5XXXXXXXXXXXXXXXXXXXXXXX')
     d hdg6            s             27    inz('HDG6XXXXXXXXXXXXXXXXXXXXXXX')
     d hdg7            s             27    inz('HDG7XXXXXXXXXXXXXXXXXXXXXXX')
     d hdg8            s             27    inz('HDG8XXXXXXXXXXXXXXXXXXXXXXX')
     d pro1            s             10    inz('PRO1XXXXXX')
     d pro2            s             10    inz('PRO2XXXXXX')
     d pro3            s             10    inz('PRO3XXXXXX')
     d pro4            s             10    inz('PRO4XXXXXX')
     d pro5            s             10    inz('PRO5XXXXXX')
     d pro6            s             10    inz('PRO6XXXXXX')
     d pro7            s             10    inz('PRO7XXXXXX')
     d pro8            s             10    inz('PRO8XXXXXX')
     d x1mn            s              3    inz('X1M')
     d x2mn            s              3    inz('X2M')
     d x3mn            s              3    inz('X3M')
     d x4mn            s              3    inz('X4M')
     d x5mn            s              3    inz('X50')
     d x6mn            s              3    inz('X6M')
     d x7mn            s              3    inz('X7M')
     d x8mn            s              3    inz('X8M')
     d x9mn            s              3    inz('X9M')
     d x10mn           s              3    inz('X10')
     d x11mn           s              3    inz('X11')
     d x12mn           s              3    inz('X12')
     d x13mn           s              3    inz('X13')
     d x14mn           s              3    inz('X14')
     d x15mn           s              3    inz('X15')
     d x16mn           s              3    inz('X16')
     d x17mn           s              3    inz('X17')
     d x18mn           s              3    inz('X18')
     d x19mn           s              3    inz('X19')
     d x20mn           s              3    inz('X20')
     d x21mn           s              3    inz('X21')
     d x22mn           s              3    inz('X22')
     d x23mn           s              3    inz('X23')
     d x24mn           s              3    inz('X24')
     d fna1            s             37    inz('FNA1XXXXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXX')
     d fna2            s             37    inz('FNA2XXXXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXX')
     d fna3            s             37    inz('FNA3XXXXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXX')
     d fna4            s             37    inz('FNA4XXXXXXXXXXXXXXXXXXXXXXXXXXX-
     d                                     XXXXXX')
     d xxterr          s              5  3 inz(99.999)
     d xxtxhd          s             13    inz('XXTXHDXXXXXXX')
     d epclas          s              5  0 inz(99999)
     d epspec          s              1    inz('A')
     d prdesc          s             30    inz('PRDESCXXXXXXXXXXXXXXXXXXXXXXXX')
     d xxarat          s              7  2 inz(99999.99)
     d xxwk90          s              9  0 inz(999999999)
     d xxline          s             15    inz('_______________')
     d xxst            s              2    inz('FL')
     d x               s              2  0
     d tax             s              8    inz('99.9999%')
     d xxdiri          s             30    inz('XXDIRIXXXXXXXXXXXXXXXXXXXXXXXX')
     d xxcurd          s              8  0 inz(99999999)
     d xxwvhd          s             20    inz('XXWVHDXXXXXXXXXXXXXX')
     d xxpage          s              3  0 inz(999)
     d pbal            s             11  2 inz(999999999.99)
     d xxemhd          s             19    inz('XXEMHDXXXXXXXXXXXXX')
     d xxexmd          s              5  4 inz(9.9999)
     d xxfdt           s              8  0 inz(99999999)
     d xxtdt           s              8  0 inz(99999999)
     d xxilpc          s              7    inz('99.999%')

     c                   seton                                        777579
     c                   seton                                        56
      *** not net rate
     c                   write     hdcnst
     c                   write     lines
     c                   write     head
     c                   do        11            x
     c                   write     det
     c                   if        x = 3
     c                   seton                                        32
     c                   else
     c                   setoff                                       32
     c                   endif
     c                   write     tdet
     c                   enddo
     c                   write     ttcnst
     c                   write     tot
     c                   write     totnot
      *** net rate
     c                   seton                                        62
     c                   write     hdcnst
     c                   write     lines1
     c                   write     head
     c                   do        11            x
     c                   write     det1
     c                   if        x = 3
     c                   seton                                        32
     c                   else
     c                   setoff                                       32
     c                   endif
     c                   write     tdet
     c                   enddo
     c                   write     ttcns1
     c                   write     tot
     c                   write     totnet
     c                   seton                                        lr

