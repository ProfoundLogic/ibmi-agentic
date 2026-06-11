     *=====================================================================
     *  (C) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
    H/TITLE IS820R1  Update DO and IE Records
     *=====================================================================
     *
     * Description: Updates Document Object and IE records with the new
     *   status passed in.
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *=====================================================================
     *  REVISIONS:
     *
     *=====================================================================

     * Index Entries Master File
     Fimiel5    uf   e           k disk

     * Document Object Master File
     Fimdol4    uf   e           k disk

     * Batch Status Table File
     Fitdsl     if   e           k disk

     * Document Notes File
     Fimdnl     if a e           k disk


     *=====================================================================
     * Prototypes
     *=====================================================================
      /copy IS820RPR


     *=====================================================================
     * Data Structure, Table & Array Definitions
     *=====================================================================
     ** Array of message line starts
     D fc              s             24    dim(5) ctdata perrcd(1)

     ** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     ** Program Information structure
     D psds          esds                  extname(szq1p)

     *=====================================================================
     * Stand Alone Fields
     *=====================================================================
     D @BatchId        s             12a
     D @BatchSts       s              2a
     D @ChangePgm      s             10a
     D @RtnCode        s              1a
     D qqfbst          s              2a

     D sequence#       s              5  0
     d xDate           s               d
     d xTime           s               t
     d dateChar        s             10a
     d timeChar        s              8a
    

     *=====================================================================
     * KFld Definitions
     *=====================================================================
     ** Batch Status Table
     C     dsky1         klist
     C                   kfld                    @BatchSts

     ** Document Notes
     C     dnky1         klist
     C                   kfld                    dodcid
     C                   kfld                    sequence#

     C     dnky2         klist
     C                   kfld                    dodcid

     ** Document Object
     C     doky1         klist
     C                   kfld                    @BatchId

     ** Index Entries
     C     ieky1         klist
     C                   kfld                    dodcid


     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** Entry parameters
     C     *entry        plist
     C                   parm                    @BatchId
     C                   parm                    @BatchSts
     C                   parm                    qqfbst
     C                   parm                    @ChangePgm
     C                   parm                    @RtnCode
    
    
     *================================================================
     * Main
     *================================================================
     C                   clear                   @RtnCode

     ** Process all docs for the batch, changing the status of any
     **  non-indexed documents
     C     doky1         chain     pdo01                              60
     C                   dow       *in60 = *off

     C                   if        doidfx = *blanks
     C                             or doidfx = 'N'

     C                   if        dodsts = qqfbst

     ** Update the Document Object file
     C                   movel     @BatchSts     dodsts
     C                   update    pdo01

     ** Write the document notes
     C                   exsr      bnotes
     C     ieky1         chain     pie01                              61

     C                   if        *in61 = *off
     ** Update the Index Entries file
     C                   movel     @BatchSts     iedsts
     C                   update    pie01
     C                   endif

     C                   endif

     C                   endif
    
     C     doky1         reade     pdo01                                  60
     C                   enddo
    
     ** End the program
     C                   move      *on           *inlr
     *===================================================================





     *===================================================================
     * BNOTES - CREATE BATCH NOTE
     *===================================================================
     C     bnotes        begsr

     C                   eval      sequence# = *hival
     C     dnky1         setgt     pdn01
     C     dnky2         readpe    pdn01                                  60
     C                   if        *in60 = *off
     C                   eval      sequence# = 1 + dnasq#
     C                   else
     C                   eval      sequence# = 1
     C                   endif

     C                   movel     dodcid        dndcid
     C                   clear                   dncm75
     C                   move      sequence#     dnasq#

     c                   callp     GetCurDate(xDate:xTime)
     c     *iso          move      xDate         dncdat
     c     *iso          move      xDate         dntrdt
     c     *hms          move      xTime         dntrtm
     C                   movel     @ChangePgm    dnpnam

     ** SECURITY VALUES NOT CURRENTLY USED ******
     C                   clear                   dnsecl
     C                   clear                   dnsbyl

     ** PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C                   movel     'H'           dnprfl

     C                   clear                   dnstat
     C                   movel     q1user        dniusr
     C                   movel     q1pgm         dnipgm
     c     *ymd          move      xDate         dnidt
     c     *hms          move      xTime         dnitim

     C                   clear                   dnlusr
     C                   clear                   dnlpgm
     C                   clear                   dnltim
     C                   clear                   dnldt

    C**** FORMAT THE COMMENT ****
     ** Line one - Batch History:
     c                   eval      dncm75 = fc(1)
     C                   write     pdn01

     ** Line two - Status:
     C                   eval      sequence# = sequence# + 1

     C                   clear                   dncm75
     C                   move      sequence#     dnasq#

     C     dsky1         chain     pds01                              61
     c                   eval      dncm75 = fc(2) + @BatchSts + ' - ' + dsldsc
     C                   write     pdn01

     ** Line three - Date/Time:
     C                   eval      sequence# = sequence# + 1

     C                   clear                   dncm75
     C                   move      sequence#     dnasq#

     c                   callp     GetCurDate(xDate:xTime)
     c     *usa          move      xDate         dateChar
     c     *usa          move      xTime         timeChar
     c                   eval      dncm75 = fc(3) + dateChar + '     ' +
     c                             timeChar

     C                   write     pdn01

     ** Line four - User:
     C                   eval      sequence# = sequence# + 1

     C                   clear                   dncm75
     C                   move      sequence#     dnasq#

     c                   eval      dncm75 = fc(4) + q1user

     C                   write     pdn01

     C     endbn         endsr
     *===================================================================



     *===================================================================
     * Table Entries
     *===================================================================
**   RMRK ENTRIES
Document History:
   Mass Chg Status   :
            Date/Time:
            User     :
            Error    :
