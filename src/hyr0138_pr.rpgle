      ******************************************************************
      *  Inline replacement for the five /COPY directives in HYR0138:
      *    /COPY *LIBL/QPRPSRC,HYR0139  - pallet-print procedures
      *    /COPY *LIBL/QPRPSRC,HYR0139A - pallet-report status logging
      *    /COPY *LIBL/QPRPSRC,HYR0552  - HYR0552 shutdown
      *    /COPY *LIBL/QPRPSRC,HYR0622  - HYR0622 shutdown
      *    /COPY *LIBL/QPRPSRC,HYR9930  - Center text utility
      *
      *  HYR0138 itself declares prototypes for HYC3512, HYC3550, HYR0152,
      *  HYR0156, HHDCSS, HHDSVI, HHDISI, HHDPCS, QCMDEXC, QMHSNDPM,
      *  QMHRMVPM, QSYGETPH, QSYRLSPH inline -- don't duplicate them here.
      ******************************************************************

      // ----- Procedures from /COPY HYR0139 -----
     D ResetDropShips  PR                  EXTPROC('RESETDROPSHIPS')
     D  Pallet#                        9P 0 CONST
     D  Order#                         8P 0 CONST

     D PalletLbl       PR                  EXTPROC('PALLETLBL')
     D  Pallet#                        9P 0 CONST
     D  Copies                         3P 0 CONST

     D sd_HYR0139      PR                  EXTPROC('SD_HYR0139')

      // ----- Procedures from /COPY HYR0139A -----
     D WrtPRLog        PR                  EXTPROC('WRTPRLOG')
     D  Pallet#                        9P 0 CONST
     D  Action                         8A   CONST

     D RtvPRSts1       PR            10A   EXTPROC('RTVPRSTS1')
     D  Pallet#                        9P 0 CONST

     D RtvPRSts2       PR            10A   EXTPROC('RTVPRSTS2')
     D  Pallet#                        9P 0 CONST

     D sd_HYR0139A     PR                  EXTPROC('SD_HYR0139A')

      // ----- Procedures from /COPY HYR0552 & HYR0622 -----
     D sd_HYR0552      PR                  EXTPROC('SD_HYR0552')
     D sd_HYR0622      PR                  EXTPROC('SD_HYR0622')

      // ----- Center (HYR9930) -- HYR0138 calls center(text) -----
     D Center          PR           256A
     D  Input                       256A   CONST
     D  Width                        10I 0 CONST OPTIONS(*NOPASS)
