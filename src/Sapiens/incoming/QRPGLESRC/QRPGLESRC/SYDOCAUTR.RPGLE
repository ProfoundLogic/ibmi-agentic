      *---------------------------------------------------------------*
      * Program:SYDOCAUTR                                          *
      * Description:Check/maintain SYDOCAUTP file                   *
      *               Checks for user IDs already processed by the    *
      *               the SYDOCAUT job                                *
      *---------------------------------------------------------------*
     FSYDOCAUTP UF A E           K Disk

     D*-------------------------------------------------------
     D*Standalone Fields
     D*-------------------------------------------------------
     D inUser          S             10
     D RtnVal          S              1

     D*-------------------------------------------------------
     D*Arrays
     D*-------------------------------------------------------

     D*-------------------------------------------------------
     D*Data Structures
     D*-------------------------------------------------------


     D @PSDS          SDS           429
     D  @PROCNAME        *PROC
     D  @PGMSTATUS       *STATUS
     D  @PRVSTATUS            16     20S 0
     D  @LINENUM              21     28
     D  @ROUTINE         *ROUTINE
     D  @PARMS           *PARMS
     D  @EXCEPTTYP            40     42
     D  @EXCEPTNUM            43     46
     D  @Reserved             47     50
     D  @WORKMSSG             51     80
     D  @PGMLIBR              81     90
     D  @RTVEXCDTA            91    170
     D  @EXCEPTID            171    174
     D  @Unused16            175    190
     D  @JOBDATE             191    198
     D  @JOBCENTRY           199    200S 0
     D  @LASTFILE            201    208
     D  @FILEINFO            209    243
     D  @WRKSTN              244    253
     D  USERID               254    263
     D  @JOBNUMBER           264    269S 0
     D  @JOBUDATE            270    275S 0
     D  @PGMRUNDAT           276    281S 0
     D  @PGMRUNTIM           282    287S 0
     D  @PGMCMPDAT           288    293
     D  @PGMCMPTIM           294    299
     D  @LVCOMPILE           300    303
     D  @SRCFILE             304    313
     D  @SRCLIBR             314    323
     D  @SRCFILMBR           324    333
     D  PGM                  334    343
     D  @MODUNAME            344    353
     D  @Unused76            354    429

     C/EJECT
     C*-------------------------------------------------------
     C*         M A I N L I N E      R O U T I N E             
     C*-------------------------------------------------------

     C     inUser        Chain     SYDOCAUTP
     C                   If        %Found(SYDOCAUTP)
     C                   Move      'Y'           Rtnval
     C                   Else
     C                   Move      'N'           Rtnval
     C                   Exsr      WRTUSR
     C                   EndIf

     C                   Eval      *INLR = *On

     C*-------------------------------------------------------
     C*SUBROUTINE:WRTUSR
     C*NARRATIVE: Write User to SYDOCAUTP file
     C*-------------------------------------------------------
     C     WRTUSR        BEGSR

     C                   Clear                   DOCUREC
     C                   Move      InUser        DOCUSR
     C                   Write     DOCUREC

     C                   EndSr

     C*-------------------------------------------------------
     C*SUBROUTINE:*INZSR
     C*NARRATIVE: Initial Program Variables
     C*-------------------------------------------------------
     C     *INZSR        BEGSR

     C     *Entry        PLIST
     C                   PARM                    inUser
     C                   PARM                    Rtnval

     C                   EndSr
