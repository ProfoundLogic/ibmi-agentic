      ******************************************************************
      *  Inline replacement for /COPY QPRPSRC,HYR0610 / HYR0614 /
      *  HYR9930 in the original Hornady source.  HYR0600 itself defines
      *  prototypes for HYR0602/0608/0138/0189/0520/VPRBLDP/HHDCSS/HHDSVI
      *  inline.  Everything else those /COPY members would have
      *  published is reconstructed here.
      ******************************************************************

      // Text utility from HYR9930
     D Center          PR           256A
     D  Input                       256A   CONST
     D  Width                        10I 0 CONST OPTIONS(*NOPASS)

      // Subprocedures from HYR0610 / HYR0614 that HYR0600 calls
     D RtvForeign      PR             1N
     D  CMCCLS                        2A   CONST
     D  GCSTCT                        2A   CONST

     D CompCustShip    PR            10I 0
     D  Grp#                          9S 0 CONST
     D  BlTo#                         7S 0 CONST
     D  ShTo#                         7S 0 CONST
     D  DShp#                         7S 0 CONST
     D  GenBOL                        1N   CONST
     D  PltCnt                        5S 0 CONST
     D  ShutDown                      1N   CONST

     D RtvCSPRO#       PR            20A
     D  DCLoc                         4A   CONST
     D  ShipVia                       2A   CONST

      // External data structure populated for IO-style calls; the fields
      // here originally came from /COPY HYR0614 in the production source.
     D IOORD#          S              8S 0
     D IOTURN          S              4S 0
     D IOAIV#          S              8S 0
     D IOALPH          S             30A
     D IOSHTO          S              7S 0
     D IOKYF1          S             20A
     D IOKYV1          S             50A
     D IOKYF2          S             20A
     D IOKYV2          S             50A
     D IORLSU          S             10A
     D IORLSD          S              7S 0
     D IORLST          S              6S 0

      // BOLReq is a PROCEDURE in HYR0614 that returns whether a BOL is
      // required for the shipment.  Called as BOLReq(ShipVia : ForeignFlag).
     D BOLReq          PR             1N
     D  ShipVia                       2A   CONST
     D  Foreign                       1N   CONST

      // Truncated identifier in error log -- "CompCus..." -- maps to
      // CompCustShip already declared above; no extra prototype needed.
