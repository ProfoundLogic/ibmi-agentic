     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H*****************************************************************
     H/TITLE SC010R - REMARKS/CUSTOM DATA FIELDS ENTRY/MAINT.
     H*****************************************************************
     H*  Program Description:                                         *
     H*  SCR01 - Add a Remark.                                        *
     H*     Accessed from F11 on SCR02                                *
     H*                   F11 on SCR03                                *
     H*                     Added code to check Remark Type for a     *
     H*                     Data Entry Program ID if DRFL03 = 'Y' in  *
     H*                     SDRPF (DARTS System Control File).        *
     H*                                                               *
     H*                     If program does exist, it is called with  *
     H*                     the following parameters:                 *
     H*                     (NOTE: It was necessary to break up the   *
     H*                     comment field of size 275 into DECOM1     *
     H*                     and DECOM2 because the largest alpha-     *
     H*                     numeric value allowed for a parameter is  *
     H*                     256.                                      *
     H*                       CALL CTDEPG                             *
     H*                         DEKEY  (40A)                          *
     H*                         DECOM1 (256A)                         *
     H*                         DECOM2 (19A)                          *
     H*                         DERTCD (1A)                           *
     H*                   NOTE: DERTCD = *BLANK is 'Add'              *
     H*  SCR02 - Main Comments Screen                                 *
     H*     Accessed from F14 of other applications                   *
     H*       Command Keys:                                           *
     H*       OV/03   Img/99   Sum/36      Cmd Key Order (CK Array)   *
     H*        Y-0      Y-1      Y-1       4-7-11-13-14               *
     H*        Y-0      Y-1      N-0       2-7-9-13-14                *
     H*        Y-0      N-0      Y-1       4-5-11-13-14               *
     H*        Y-0      N-0      N-0       2-5-9-13-14                *
     H*        N-1      N-0      N-0       1-5-9-13-14                *
     H*        N-1      N-0      Y-1       3-5-11-13-14               *
     H*        N-1      Y-1      N-0       1-7-9-13-14                *
     H*        N-1      Y-1      Y-1       3-7-11-13-14               *
     H*     Accessed from F13 on SCR03                                *
     H*       Cmd Key Display (CK Array Element)                      *
     H*         5  goto  6                                            *
     H*         7  goto  8                                            *
     H*         9  goto 10                                            *
     H*        11  goto 12                                            *
     H*  SCR03 - Database Entry/Update and limited Comments Screen    *
     H*     Accessed from F13 on SCR02                                *
     H*       Command Keys:                                           *
     H*       OV/03   Img/99   Sum/36      Cmd Key Order (CK Array)   *
     H*        Y-0      Y-1      Y-1       4-8-12-13                  *
     H*        Y-0      Y-1      N-0       2-8-10-13                  *
     H*        Y-0      N-0      Y-1       4-6-12-13                  *
     H*        Y-0      N-0      N-0       2-6-10-13                  *
     H*        N-1      N-0      N-0       1-6-10-13                  *
     H*        N-1      N-0      Y-1       3-6-12-13                  *
     H*        N-1      Y-1      N-0       1-8-10-13                  *
     H*        N-1      Y-1      Y-1       3-8-12-13                  *
     H*     Accessed from F13 on SCR02                                *
     H*       Cmd Key Display (CK Array Element)                      *
     H*         6  goto  5                                            *
     H*         8  goto  7                                            *
     H*        10  goto  9                                            *
     H*        12  goto 11                                            *
     H*  SCR04 - "User is not enrolled in DARTS" screen               *
     H*     Accessed from F14 of other applications                   *
     H*  SCR05 - "This function is not available..." screen           *
     H*     Accessed from                                             *
     H*  SCR06 - Work with Remarks/Documents/Image                    *
     H*     Accessed from F5 on SCR02                                 *
     H*                   F5 on SCR03                                 *
     H*                     Added code to check Data Entry Interface  *
     H*                       Data Entry Interface flag: CMFL01 = Y   *
     H*                       Data Entry Pgm Name:       CMDCID       *
     H*                     CALL DEPGM with the following:            *
     H*                        Option 2 ('Edit Doc/Data')             *
     H*                           DERTCD = '2'    is 'Update'         *
     H*                        Option 5 ('View Doc/Data')             *
     H*                           DERTCD = '1'    is 'View'           *
     H*===============================================================*
     H** REVISIONS:
     H*
     H* 08/03/13  C:33  Implement GS (no comments)
     H*
     H*  10/16/12 C:087 Project: wc2115
     H*                 Removed call to vDARTS as this is not needed
     H*                 in v13+ with Profound UI
     H*
     H*  10/17/11 C:277 Project: SY195
     H*                 QDLS migration to IFS
     H*                 Add two parameters with command
     H*
     H* 02/04/09 C:062 Seq#: 67019                                    *
     H*                DON'T GIVE ERROR IF DEFAULT DOESN'T HIT.       *
     H*
     H* 01/26/09 C:189 Seq#: 66931                                    *
     H*                WMEMP should be open ONLY with client 93       *
     H*                                                               *
     H*
     H* 07/24/08 C:33  Seq#: DR065/64489                              *
     H*                Inform the user that the diary type does not   *
     H*                 match the dart code default if the user       *
     H*                 manually changes the code.                    *
     H*
     H* 05/21/08 C:136 PROJECT: 93WHI
     H*                DISPLAY VIP MEMBER VERBIAGE IN HEADER
     H*
     H* 05/07/07 C:180 SUPPORT: WA133 56379
     H*                Do not allow document id for policy pages to be
     H*                updated while document creation is still in
     H*                progress by PGM W7431R.
     H*
     H***  04/17/07 C:02 PJCT: 89AIH
     H*      IF DRFL13 IS NOT ' ', AND THE DOC BEING VIEWED IS OTHER THAN OV
     H*        (CMIMGF = 'O'), USE RUNRMTCMD TO SEND THE ITEM TO A PC FOR
     H****     EXECUTION UNLESS DRFL13 IS AN "S" THEN USE STRPCCMD
     H* 03/03/05 C:136 SUPPORT: DR114 35807 (114WAM)
     H*                POPULATE GROUP FIELD IN SMHDP
     H*
     H* 06/14/04 C:062 Project: 129WCS
     H*                GET THE DE PGM FROM SDAIP IF EXISTS
     H*
     H***  05/08/04 C:02 PJCT: BASE ENHANCEMENT
     H*      IF DRFL13 IS NOT ' ', AND THE DOC BEING VIEWED IS OTHER THAN OV
     H*        (CMIMGF = 'O'), USE RUNRMTCMD TO SEND THE ITEM TO A PC FOR
     H****     EXECUTION
     H*
     H* 11/25/03 C:128 Project: DR081 #40106                          *
     H*                If a user presses F8 instead of Shift F8 the   *
     H*                image flag would not be cleared. Cleared
     H*                SCIMGF in the DLTSCN subroutine.
     H*
     H* 05/28/03 C:128 Project: DR181                                 *
     H*                In the file SCDRP if the field DRFL20='Y' then *
     H*                Do not allow the user to enter a date earlier
     H*                than todays date.
     H*
     H* 11/06/02 C:128 Project: DR094 #34620                          *
     H*                Leave ARSEQ# intact when deleting a remark.    *
     H*                This is done in order to keep addional remarks
     H*                in sync. with SDCMP.
     H*
     H* 04/02/02 C:128 Project: VI081 #31781                          *
     H*                Leave CMSEQ# intact when deleting a remark.    *
     H*                This is done in order to create a Unique.
     H*                Key in SDCMP.
     H*
     H* 05/22/01 C:128 Project: DR111 #27565                          *
     H*                *in28 used both for a error and as a hi int.   *
     H*                image for a closed diary item. Changed the
     H*                hi int. image to use *in97. *in97 is used in
     H*                only one place and it is used as a CHAIN ind.
     H*
     H* 02/01/01 C:33  PROJECT: DR143                                 *
     H*                ALLOW FOR MULTI-GROUP PROCESSING.              *
     H*
     H*  11/13/00 C:128 Project: DR137
     H*                 Program was not editing screen 1 (SCRN01)
     H*                 when the Image application flag was set to 'Y'.
     H*                 Changed to use the F key (F8=Scan Image) as
     H*                 the decision maker instead of *in99.
     H*
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 03/23/00 C:113 PROJECT: SUPPORT #21733                        *
     H*                Corrected the edits for dates in custom info.  *
     H*                                                               *
     H* 02/07/00 C:113 PROJECT: DR096                                 *
     H*                Changed to default the remark security level   *
     H*                to the remark code's default security level    *
     H*                when adding a new remark.                      *
     H*                                                               *
     H* 11/19/99 C:113 Changed to check for installation of VDARTS    *
     H*                and check users security access to same.       *
     H*                                                               *
     H* 10/08/99 C:33  PROJECT: WC922                                 *
     H*                AVOID RECURSIVE CALLS.                         *
     H*                                                               *
     H* 09/23/99 C:118 SUPPORT: MC087/#18202                          *
     H*                Changed indicators in display file to not      *
     H*                display SCSEL on SCR02 if there are no remarks.*
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 05/27/99 C:33  SEQ#: DR048/16735                              *
     H*                AVOID RECURSSIVE CALLS.                        *
     H*                                                               *
     H* 05/18/99 C:58  Support: 17233, 17594, 16802                   *
     H*                Clear DCID before updating, prevent            *
     H*                residual data.                                 *
     H*                                                               *
     H* 02/10/99 C:31  Support: 16401                                 *
     H*                DARTS comment not clearing                     *
     H*                1. Clear DECOM1 & DECOM2 after write           *
     H*                2. Add move to SCCOMM after SC305R             *
     H*                                                               *
     H* 12/03/98 C:94  PROJECT: DR073                                 *
     H*                REMARK TYPE SEARCH - REPOSITION.  MAKE F10 SEARCH
     H*                SO THAT THEY DO NOT HAVE TO TYPE IN A ? AND THEN*
     H*                F10 TO SEARCH.                                 *
     H*                                                               *
     H* 12/02/98 C:94  PROJECT: SUP DR048/15029 & DR033/14953         *
     H*                WHEN DIARY TYPE IS PROTECTED AND THE USER HAS  *
     H*                A DEFAULT REMARK TYPE, IF THEY CHANGE THE      *
     H*                REMARK TYPE MANUALLY, IT WILL CHAIN BACK OUT TO*
     H*                STCTP TO GET THE CORRECT DIARY TYPE.           *
     H*                                                               *
     H* 11/23/98 C:53 PROJECT: DRMNT                                  *
     H*               CORRECT RECURSIVE CALL FOR WCMC PGM             *
     H*                                                               *
     H* 10/27/98 C:56 PROJECT: DR068 -  REFERENCE SUPPORT DR048 #14574*
     H*               EDIT DIARY USER FIELD ALL THE TIME              *
     H*                                                               *
     H* 10/12/98 C:98 PROJECT: 48WML                                  *
     H*               ADD ACCESS RESTRICTIONS BASED UPON SMSLP/SLRACC *
     H*                                                               *
     H* 10/01/98 C:94 SUPPORT: DR090/13891                            *
     H*               CHANGE SLF03 TO SLF01 IN ORDER TO ALLOW         *
     H*               RESTRICTION TO F1                               *
     H*                                                               *
     H* 09/28/98 C:02 IMAGE: USE IMAGE DOC ID ON CALL TO SCIMGP       *
     H*                                                               *
     H* 09/15/98 C:56 SUPPORT: DR065/13775                            *
     H*               DEFAULT DIARY TYPE TO USERS DIARY TYPE          *
     H*                                                               *
     H* 08/31/98 C:15 SUPPORT: DR060/13023                            *
     H*               Removed code which calls the data entry pgm when*
     H*               adding a diary and the remark date is less than *
     H*               or equal to the system date.                    *
     H*                                                               *
     H* 08/26/98 C:56 PROJECT: DRMNT                                  *
     H*               EDIT AGAINST INACTIVE REMARK TYPR CODE          *
     H*                                                               *
     H* 08/26/98 C:62 PROJECT: DR057                                  *
     H*               DEFAULT DIARY TYPE BY REMARK CODE.              *
     H*                                                               *
     H* 07/25/98 C:02 Support: 12611                                  *
     H*               ADDED PARM FROM SYSCTL TO THE DSPDOC, PRTDOC, AND
     H*               EDTDOC CALLS FOR MICROSOFT PRODUCTS.            *
     H*                                                               *
     H*                                                               *
     H* 07/21/98 C:69 Support: 12666                                  *
     H*               Changed the way the check for a recursive call  *
     H*               is done                                         *
     H*                                                               *
     H* 07/17/98  C:98  PROJECT: DR057                                *
     H*                 ADD INDICATOR 38 TO PROTECT DIARY TYPE BASED  *
     H*                 ON SYSTEM FLAG DRFL08                         *
     H*                 ADD INDICATOR 39 TO PROTECT PURGEABLE ITEM    *
     H*                 BASED ON SYSTEM FLAG DRFL09                   *
     H*                                                               *
     H*****************************************************************
     H* 11/20/90 LJD    - TO MEET CLIENTS' NEEDS OF SEEING REMARKS IN *
     H*                   ORDER OF MOST RECENT DATE TO OLDEST DATE, THE
     H*                   LOGICAL SDCML1 WAS REDONE WITH KEY CDAT AS  *
     H*                   DESCENDING.  SECTION IN CODE WHERE DATE SORT*
     H*                   IS DONE HAS BEEN REVISED TO BEGIN AT 99999  *
     H*                   IF NO DATE SELECTION WAS MADE               *
     H* 02/14/91 MDG    - CHANGE - NO EDIT AGAINST SEC FILE *** INSTEAD
     H*                   EDIT AGAINST SYSTMEM USER PROFILES*         *
     H* 03/26/91 MDG    - CHANGE - ONLY EDIT AGNST SEC FILE IF A DIARY*
     H*                   TYPE - (LET IT BE BLANK)                    *
     H* 03/26/91 MDG    - CHANGE - USE USER PROFILE SEARCH INSTEAD OF *
     H*                   SECURITY SEARCH.                            *
     H* 04/12/91 MDG    - CHANGE - ADD CODE TO ALLOW SUPPORT OF IMAGE *
     H*                   SCAN,VIEW, AND PRINT                        *
     H*                                                               *
     H* 07/01/91 LJQ    - CL003 mod -Add F20 to SCR01 to auto set/create
     H*                   folder and/or document according to remark  *
     H*                   type. CALL USER EXIT PROGRAM                *
     H*                   *IN22, *IN23, *IN24, *IN25 are used for error
     H*                   messages.                                   *
     H*        #$#$#$#$$# IF DARTS IS ADDED TO ANY PROGRAM FOR        *
     H*            CLIENT #3; THEN A DATA STRUCTURE MUST BE           *
     H*            ADDED TO EXIT PGM FOR THAT PROGRAM SO THAT THE AUTO-
     H*            CREATE FOLDER/DOCUMENT FUNCTION WILL WORK.         *
     H*            ALSO; AREA OF SERVICE FOR FOLDERS IS HARD          *
     H*            CODED; IF ADDITIONAL AREAS ARE ADDED THEY          *
     H*            MUST BE UPDATED IN THIS PROGRAM.                   *
     H*                                                               *
     H* 07/12/91 MDG    - CHANGE - ADD CODE TO ALLOW ADDITIONAL COMMENTS
     H*                   FOR EXISTING COMMENT.                       *
     H* 07/15/91 LJQ    - CHANGE - DEFAULT FOLDER SHOULD BE USED FROM *
     H*                   SCREEN IN STEAD OF SYSTEM SECURITY FILE (SMSLL)
     H*   Added processing to allow display/entry of different record *
     H*      record formats:  "A" = 10A/6.0N/9.2N    vs. "B" = 35A    *
     H*      Subroutines:         HDGET                      H1GET    *
     H*                           CGGETA                     CGGETB   *
     H*                           WRITEA                     WRITEB   *
     H*   Added new file SDH1P for data file of record format "B"     *
     H*   Had to change parms to:   "Special Edit Program",SC520R     *
     H*   Added call to new Table Search Pgm SC553R for Rcd Format "B"*
     H*                                                               *
     H*  09/20/91 TMN   - SAVE VALUE OF INDICATOR 55, CLEAR IT BEFORE *
     H*                   ENTERING SCR06 & SCR01 FROM SCR02 & SCR03,  *
     H*                   AND RETURN ITS VALUE WHEN EXITING SCR06 & SCR01.
     H*                   ALSO RETRIEVE VALUE OF *IN55 IN RELOD SUBROUTINE.
     H**  1.  ADD WRITE TO CMIPGM (SDCMP) DARTS COMMENT FILE          *
     H*                                                               *
     H*  12/18/91 LJD   - Proj: SC082                                 *
     H*                   1.  Fix Custom Group ID Entry/Update - F10 Search
     H*                       a.  Fix Parms passed to SC550R          *
     H*                       b.  Because the date & numeric searching*is not
     H*                           working properly, commented out this*code
     H*                 - Proj: SC021                                 *
     H*                   1.  Chgd messages displayed on SCR04 & SCR05*in DSPF
     H*                   2.  Added condition to F3 for SCR04 & SCR05 *
     H*                 - Proj: SC078                                 *
     H*                   1.  Added condition to SCR06/Option 6:      *
     H*                             Document ID cannot be blank       *
     H*                 - Proj: SC083                                 *
     H*                   1.  Added variable XXOFIF to control AS/400 Office
     H*                       processing.  If XXOFIF = 'N', and user chose
     H*                       an Office Function, indicator 31 turned on.
     H*                       Added to prevent "bombs" on Office processing
     H*                 - Proj: SC080                                 *
     H*                   1.  Corrected problem that if no Default Folder
     H*                       entered...edit on Document ID bombed.   *
     H*                 - Proj: SC085                                 *
     H*                   1.  Allow view/add of Additional Remarks from SCR02:
     H*                       a.  shortened "DATE:" to "DT:"          *
     H*                       b.  no heading for selection field      *
     H*                       c.  7=Addtl Remarks                     *
     H*                       d.  "Options" at top of screen next to search
     H*                           selections                          *
     H*                 - Proj: SC090                                 *
     H*                   1.  Pass PNAM as a parm into SC014R to update ARPNAM
     H*                       with keyed program name                 *
     H*                                                               *
     H*  02/19/92 LJD   - Proj: SC088                                 *
     H*                   1.  Revamp of DARTS Report - F19 Print Remarks
     H*                       a.  New Print Pgm - SC710R with new parms
     H*                                                               *
     H*  03/20/92 WLD   - Proj: AND                                   *
     H*                   1.  Added "Purgeable item" field to SCR01 (Add
     H*                       Remark)                                 *
     H*                       *IN34, *IN35, *IN37, *IN47 used for error
     H*                       messages.                               *
     H*                                                               *
     H*  03/20/92 WLD   - Proj: AJG                                   *
     H*                   1.  Change "Add a Remark" (SCR01) to Require*
     H*                       the entry of the "Type" Field.          *
     H*                                                               *
     H*  03/23/92 WLD   - Proj: AJP                                   *
     H*                   1.  Changed Remark Type to be 5A instead of *
     H*                       2A.  Adjusted all related code.         *
     H*                                                               *
     H*  03/27/92 WLD   - Proj: AJP                                   *
     H*                   1.  Changed screen to divide the remark code*
     H*                       into department and remark type.        *
     H*                                                               *
     H*  04/06/92 LJD   - Proj: ANI                                   *
     H*                   1.  Added F1 = OV/400 based upon program name
     H*                       in DARTS System Control File SDRPF/DRAL02
     H*                   2.  Revamped CK and CK1 Command Key arrays  *
     H*                       to incorporate F1=OV/400 when used      *
     H*                                                               *
     H*  04/07/92 LJD   - Proj: AJZ                                   *
     H*                   1.  Added QQKEY parm to parm list for call to
     H*                       "Work with Diary Items" program SC015R  *
     H*                                                               *
     H*  04/09/92 WLD   - Proj: ANG                                   *
     H*                   1.  SCR01- 'Add a Remark'                   *
     H*                       Added code to check Remark Type for a   *
     H*                       Data Entry Program ID if DRFL03 = 'Y' in*
     H*                       SDRPF (DARTS System Control File).      *
     H*                       If program does exist, it is called.    *
     H*                       *IN42 is a global indicator set to a 1  *
     H*                       if the Data Entry program option is     *
     H*                       installed.                              *
     H*                       *IN26, and *IN27 are used for error msgs*
     H*                                                               *
     H*                   2.  SCR06- 'Work with Remarks'              *
     H*                       Added code to check Data Entry Interface*
     H*                       flag (CMFL01 = 'Y') when an option 2 or *
     H*                       Option 5 was chosen                     *
     H*                                                               *
     H*  04/20/92 WLD   - Proj: AJP                                   *
     H*                   1.  Changed program so that when user pressess
     H*                       F14 to enter DARTS(SCR02), instead of the
     H*                       remarks displaying in the order they were
     H*                       entered, they are positioned beginning  *
     H*                       with the user's "Default Remark Prefix" *
     H*                       (from Security file).  This order should*
     H*                       carry through to SCR03 and SCR06.       *
     H*                                                               *
     H*  04/21/92 WLD   - Proj: ANF                                   *
     H*                   1.  Changed program so that if Summary Code *
     H*                       option is installed, user will have the *
     H*                       ability to enter a summary code on the  *
     H*                       following screens: SCR02, SCR03, SCR06. *
     H*                       Only the Remark Codes that have this    *
     H*                       corresponding Summary Code will be      *
     H*                       displayed.  They will be displayed in   *
     H*                       Remark Code order. Must use F14 to order*
     H*                       *IN36 is set on '1' if summary code     *
     H*                       option is installed (and set off '0' if *
     H*                       it is not installed).                   *
     H*                       *IN91 is used for invalid summary code  *
     H*                       message.  *IN92, *IN93, *IN94 are used  *
     H*                       to indicate which screen the summary    *
     H*                       code grouping was requested from.       *
     H*                                                               *
     H*  04/24/92 WLD   - Proj: AJP                                   *
     H*                   1.  Changed F20 on screen one (Set Default  *
     H*                       Folder and Document ID - Auto Create).  *
     H*                       Check to see if 'Sub-Area Code exists'  *
     H*                                                               *
     H*  04/30/92 WLD   - Proj: ANG                                   *
     H*                   1.  Changed "Add a Remark" (SCR01) so that  *
     H*                       upon return from a Data Entry Program,  *
     H*                       the program will return to SCR01 and    *
     H*                       allow additional remarks to be entered. *
     H*                                                               *
     H*  05/06/92 WLD   - Proj:                                       *
     H*                   1.  Added code to "Work with Remarks" screen*
     H*                       for deleting a remark.  When a remark is*
     H*                       deleted, its status is set to 'I'.  Added
     H*                       code to set the status of the           *
     H*                       corresponding additional remarks to 'I' *
     H*                       also.                                   *
     H*                                                               *
     H*  06/17/92 WLD   - Proj:                                       *
     H*                   1.  Changed *IN42 as global indicator for   *
     H*                       designating that Data Entry Program ID  *
     H*                       is installed because it is being used on*
     H*                       SCR03 for an error message.  Changed    *
     H*                       indicators *IN34, *IN35, *IN47, and     *
     H*                       *IN37 which were being used on SCR01    *
     H*                       for error messages, to *IN42, *IN43,    *
     H*                       *IN44, and *IN45 which are only being   *
     H*                       used on SCR03.  This will "free-up"     *
     H*                       indicators *IN34, *IN35, and *IN37      *
     H*                       for use as global variables.  Currently *
     H*                       they exist in SC010F on SCR03 but do    *
     H*                       not seem to be used in the RPG code.    *
     H*                       I have commented them out in SC010F     *
     H*                       for now.                                *
     H*                                                               *
     H*  06/19/92 WLD   - Proj:                                       *
     H*                   1.  Fixed problem with "Add a Remark"       *
     H*                       When F20 was being called with the      *
     H*                       user's default remark code (from security
     H*                       file), was getting error "Remark type   *
     H*                       must be entered and valid to set folder/*
     H*                       document."                              *
     H*                                                               *
     H*  07/23/92 WLD   - Proj:                                       *
     H*                                                               *
     H*                   1.  Zero out CMSEQ# and ARSEQ# on delete.   *s
     H*                                                               *
     H*  07/29/92 WLD   - Proj: AJG                                   *
     H*                                                               *
     H*                   1.  Change initial order of remarks back to *s
     H*                       remark date(descending) and remove the  *
     H*                       code that initially orders the remarks  *
     H*                       starting with the user's remark code    *
     H*                       in the security file.                   *
     H*                                                               *
     H*  08/31/92 WLD   - Proj: ANJ                                   *
     H*                                                               *
     H*                   1.  Changed program sections that allow an  *
     H*                       option to be chosen to position cursor  *
     H*                       on selected item upon return from       *
     H*                       option chosen.                          *
     H*                                                               *
     H*  09/01/92 WLD   - Proj: ANG                                   *
     H*                                                               *
     H*                   1.  Used indicator *IN37 for call to Data   *
     H*                       Entry programs so that program would not*
     H*                       bomb if the Data Entry program does not *
     H*                       exist.                                  *
     H*                                                               *
     H*  09/02/92 WLD   - Proj: ANG                                   *
     H*                                                               *
     H*                   1.  Changed 'Add a Remark' (SCR01) so that  *
     H*                       a Remark Type that has a Data Entry     *
     H*                       program associated with it is allowed to*
     H*                       be assigned to a diary type remark.     *
     H*                                                               *
     H*  09/03/92 WLD   - Proj: ???                                   *
     H*                                                               *
     H*                   1.  Changed User search from SC650F/R       *
     H*                       (User Profile Name Search) to SC530F/R  *
     H*                       (DARTS Security File Search).           *
     H*                                                               *
     H*                   2.  Changed edit for 'diary to' user field  *
     H*                       for 'Add a Remark' (SCR01).  It is now  *
     H*                       editted against SMSLP instead of user   *
     H*                       profile.                                *
     H*                                                               *
     H*  09/08/92 WLD   - Proj: AMN                                   *
     H*                                                               *
     H*                   1.  In 'Add a Remark', move QQKEY into DEKEY*
     H*                       for call to DEPGM -- per Jackie.        *
     H*                                                               *
     H*  09/10/92 WLD   - Proj: ANG                                   *
     H*                                                               *
     H*                   1.  Made changes to error handling for call *
     H*                       to DEPGM.  Added code to monitor for    *
     H*                       a recursive call and for the message    *
     H*                       given if the program is not found in    *
     H*                       the library list.                       *
     H*                       Used indicators *IN47, *IN48, *IN49     *
     H*                                                               *
     H*  12/21/92 C:10  - Proj: ANG                                   *
     H*                                                               *
     H*                   1.  On 'Add a Remark' screen, corrected     *
     H*                       problem where recursive call message was*
     H*                       displaying for a data entry program when*
     H*                       date entered was > system date.  Changed*
     H*                       it to allow DARTS remark to be added in *
     H*                       this case.                              *
     H*                                                               *
     H*                   2.  Added code to check for recursive call to
     H*                       Data Entry Program on the 'Work with    *
     H*                       Remarks' screen.  This code was added to*
     H*                       the Edit and View options (SCSEL = 2,   *
     H*                       SCSEL = 5)                              *
     H*                                                               *
     H*  01/13/93 C:10  - Proj: ASR                                   *
     H*                                                               *
     H*                 1.  Added recursive call error handling to    *
     H*                     F6=Work with Diary Items                  *
     H*                                                               *
     H*                                                               *
     H*  05/10/93 C:02  - Proj: OPT                                   *
     H*                                                               *
     H*                 1.  Added CODE TO PERFORM OPTICAL GET IF DOC LOCATION
     H*                     IS ON OPTICAL                             *
     H*  05/11/93 C:12  - Proj:                                       *
     H*                                                               *
     H*                 1.  CHANGED INVALID SELECTION INDICATOR ON SCR02 FROM
     H*                     *IN55 TO *IN18 TO CORRECT INDICATOR PROBLEM
     H*                                                               *
     H*  08/04/93 C:10  - Proj: ATD                                   *
     H*                                                               *
     H*                 1.  Changed edit on 'Add a Remark' (SCR01) to allow
     H*                     the purge flag to be 'Y' if there is a document
     H*                     attached.  Removed code that automatically*
     H*                     assigns 'N' to purgeable flag (regardless of what
     H*                     the user enters) if a remark code with an *
     H*                     associated Data Entry Program is assigned.*
     H*                                                               *
     H*  10/22/93 C:20  - REQ/PROJ: DRMNT                             *
     H*                                                               *
     H*                 1.  WHEN REMARK TYPE IS PULLED FROM F10 ON SCR01 LOAD
     H*                     THE CORRESPONDING DIARY TYPE FLAG.        *
     H*                                                               *
     H*  01/25/94 RW25  - PROJ: DR002                                 *
     H*                                                               *
     H*                 1.  BLANKED OUT SUMMARY CODE AND DEFINEDED    *
     H*                     SUMMARY CODE                              *
     H*                 2.  ADDED SUMMARY CODE TO THE CALL PARM LIST  *
     H*                     FOR SC711R                                *
     H*                                                               *
     H*  03/17/94 C:28  - PROJ: DR003                                 *
     H*                 1.  ADDED INDICATOR TO EDIT DIARY TYPE REQUIRE:
     H*                     MUST BE 'Y' OR 'N'.                       *
     H*                                                               *
     H*  03/29/94 C:20  - PROJ: DRMNT                                 *
     H*                 1.  MODIFICATION TO GET SEQUENCE NUMBER DIRECTLY
     H*                     BEFORE WRITING DARTS REMARK               *
     H*                 2.  ADDED FILE SDCMLI(CMKEY/CMSEQ# ONLY)      *
     H*                                                               *
     H*  08/25/95 C:38  - PROJ: DR010                                 *
     H*                 1.  MODIFICATION TO GIVE SECURITY LEVEL NUMBER*Y
     H*                     A DEFAULT VALUE IN THE'ADD REMARK' OPTION.*
     H*                     F-23 BASED ON A DEFAULT SECURITY LEVEL    *
     H*                     VALUE THAT WILL BE SET IN THE REMARKS     *
     H*                     MAINTENANCE PROGRAM AND STORED IN         *
     H*                     FILE STCTP.  IF ONE IS NOT AVAILABLE, THE *
     H*                     USER SECURITY LEVEL NUMBER IS ASSIGNED AS *
     H*                     THE VALUE.  THE VALUE MAY BE CHANGED TO ANY
     H*                     INTEGER VALUE BETWEEN (001 AND 999 ONLY.) *
     H*                     THE STCTP FILE IS KEYED TO CTKY=CMRMCD(5A).
     H*                                                               *
     H*                 2.  INDICATOR *IN59 WAS ADDED TO SCREEN SCR01.*
     H*                     IT IS USED TO INDICATE CHAIN MATCH = '0'  *
     H*                                               NO MATCH = '1'. *
     H*                                                               *
     H*  09/20/95 C:30  - PROJ: DR012                                 *
     H*           Modify the display of remarks to highlight the      *
     H*           Remark Date if it is a Completed Diary Remark.      *
     H*                                                               *
     H*  09/22/95 C:30  - PROJ: DR014                                 *
     H*           Modify the SCR06 display to include the Remark      *
     H*           SEQ#.                                               *
     H*                                                               *
     H*  09/25/95 C:30  - PROJ: DR015                                 *
     H*           Add option 'P' to Print a single DARTS Remark via   *
     H*           batch job submission.                               *
     H*                                                               *
     H*  10/31/96 C:02  - PROJ: DR008                                 *
     H*           CHANGES FOR EDIT PROGRAMS TO WORK AND EDITS ON DB FLDS
     H*                                                               *
     H*                                                               *
     H*  07/18/96 C:53  - PROJ: DR021                                 *
     H*           CHANGE DATA STRUCTURE NAMES TO ACTUALLY REFLECT FIELDS
     H*           IN FILE FOR USER EDIT PGMS.                         *
     H*           CHANGE CURSOR TO POSITION AT THE TOP OF SCREEN.     *
     H*                                                               *
     H*  09/12/97 C:30    Project:  DR015  Support#:  8565            *
     H*           Change QUE assignment to use User Parm OUTQ.        *
     H*                                                               *
     H*  01/05/98 C:02    PROJ: ISSIMG                                *
     H*   MODIFIED THE PARAMETERS TO PASS TO THE SCIMGV PROGRAM       *
     H*   PASSING IN THE KEY, SEQ#, DCID , AND FOLD                   *
     H*                                                               *
     H*                                                               *
     H*  01/05/98 C:02    PROJ: ISSIMG                                *
     H*   MODIFIED THE PARAMETERS TO PASS TO THE SCIMGS PROGRAM       *
     H*   PASSING IN THE SEQ#, AND WRITING THE DARTS RECORD BEFORE SCAN
     H*   THEN UPDATING, OR DELETING THE RECORD UPON RETURN           *
     H*                                                               *
     H*   1/5/98 C:02    PROJ: ISSIMG                                 *
     H*   REMOVED THE VALIDITY CHK AGAINST THE IMAGE.... ALLOW IMAGES T
     H*   BE DELETED JUST AS ANY OTHER DARTS REMAR >>>                *
     H*                                                               *
     H*   02/12/98    C: 86    PROJ: DR091    SEQ#10215               *
     H*       CHANGED TO SDCMLM AS THE PRIMARY LOGICAL SO REMARKS WILL*
     H*       BE IN DESCENDING ORDER BY DATE AND TIME                 *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*   02/26/98    C: 86    PROJ: DR091    SEQ#10T95               *
     H*       ADDED CODE, WHEN F11 IS ON AND *IN60 IS OFF, MOVE DEFAULT
     H*       OF 'N' INTO DIARY TYPE.                                 *
     H*                                                               *
     H*  03/27/98    C: 86    PROJ: DR095    SEQ#10791                *
     H*       ON F11, ADD REMARK, CHANGED SEARCH REMARK TYPE FROM     *
     H*               TO SC305R.                                      *
     H* ------------------------------------------------------------- *
     H*  NOTE: ADD NEW COMMENTS TO THE TOP OF REVISIONS.              *
     H* ------------------------------------------------------------- *
     H*===============================================================*
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* SCR01:                                                        *
     H*  03       Office NOT installed: Nondisplay of F1=OV/400       *
     H* N03       Office IS installed:  Display/Use of F1 = OV/400    *
     H*  06      NON DISPLAY ON DOC ID/TYPE/DATE HEADINGS             *
     H*  07      GLOBAL - COMMAND KEY REQUESTED                       *
     H*  20      ???                                                  *
     H* N20      ???                                                  *
     H*  21      SCM0531-UNABLE TO PROCESS FOR THIS FOLDER            *
     H*  22      SCM0545-NOT ABLE TO BUILD FOLDER (AUTO-CREATE)       *
     H*  23      SCM0546-NOT ABLE TO BUILD DOCUMENT (AUTO-CREATE)     *
     H*  24      SCM0547-REMARK TYPE MUST BE VALID FOR AUTO-CREATE    *
     H*  25      SCM0548-SET FOLDER/DOC INVALID FOR INCOMING PROGRAM  *
     H*  26      SCM0609-DOCUMENT ID IS NOT ALLOWED WITH A REMARK TYPE*
     H*          THAT HAS AN ASSOCIATED DATA ENTRY PROGRAM ID.        *
     H*  27      SCM0   -NO SUB-AREA EXISTS FOR THIS REMARK TYPE.     *
     H*  31      GLOBAL - SCM0550-AS/400 Officevision is not installed*
     H*  32      SCM0532-Default Folder required if Document ID indicated
     H*  33             -ERROR OCCURED ON SCAN OPERATION              *
     H*  34      GLOBAL - DATA ENTRY PROGRAM OPTION IS INSTALLED      *
     H*  35      SCM0502-VALUE MUST BE 'Y' OR 'N'                     *
     H*  37      GLOBAL - Used on call to Data Entry Program (DEPGM)  *
     H*  38      DO NOT ALLOW CHANGE OF DIARY TYPE - PROTECT          *
     H* N38      ALLOW CHANGE OF DIARY TYPE                           *
     H*  39      DO NOT ALLOW CHANGE OF PURGABLE ITEM - PROTECT       *
     H* N39      ALLOW CHANGE OF PURGEABLE ITEM                       *
     H*  42      SCM0603-VALUE MUST BE 'Y', 'N', OR BLANK.            *
     H*  43      SCM0604-THIS REMARK TYPE IS NON-PURGEABLE.           *
     H*  44      SCM0605-DIARY TYPE MUST BE 'Y' IN ORDER FOR THIS     *
     H*          REMARK TO BE PURGEABLE.                              *
     H*  46      SCM0502-                                             *
     H*  47    SCM0619-Must find alternative access to this data entry*
     H*                program.                                       *
     H*  48    SCM0620-Data entry program is not available at this    *
     H*                time.                                          *
     H*  51      SCM0501-IF ENTERED, VALUE ENTERED IS NOT DEFINED IN TABLE FILE
     H*  52      SCM0500-INVALID DATE ENTERED                         *
     H*  53      SCM0503-VALUE CANNOT BE EQUAL TO BLANKS - REMARK REQUIRED
     H*  54      SCM0516-IF ENTERED, DOCUMENT NOT FOUND IN SPECIFIED FOLDER
     H*  55      SCM0512-INVALID REMARK CODE ENTERED                  *
     H*  56      DISPLAY "UNSECURE REMARK", REMARK IS SECURED         *
     H* N56      DISPLAY "SECURE REMARK", REMARK IS UNSECURED         *
     H*  57      SCM0532-FOLDER MUST BE ENTERED IF DOCUMENT IS ENTERED*
     H*  58      SCM0612-User is not enrolled in DARTS. See your      *
     H*          System Administrator.                                *
     H*  59      VALIDATE CHAIN COMMAND MATCH FOR DEFAULT SECURITY LVL.
     H*  60      GLOBAL - GENERAL CHAIN: PSL01, PCM01, PCG01          *EDIT PGM
     H*  91      SCM0525-DOCUMENT NAME MUST BE VALID TO PERFORM ADD FUNCTION
     H*  92      SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER         *
     H*  93      SCM0518-USER NOT ENROLLED IN OFFICE 400              *
     H*  94      SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT  *
     H*  95      SCM0520-DOCUMENT NAME ALREADY EXISTS AS DOCUMENT OR FOLDER
     H*  96      SCM0521-FOLDER NOT FOUND                             *
     H*  97      GLOBAL - GENERAL CHAIN: PCM0A                        *
     H* N98      EITHER NO SECURITY OR MEETS SECURITY LEVEL           *
     H*  98      PROTECT REMARK TYPE BECAUSE OF SECURITY LEVEL        *
     H*  99      GLOBAL - IMAGE PRODUCT IS INSTALLED                  *
     H* SCR02:                                                        *
     H*  01      SFLDSP/SFLDSPCTL                                     *
     H*  02      IMAGE TYPE REMARK RECORD IS DISPLAYED                *
     H*  03       Office NOT installed: Nondisplay of F1=OV/400       *
     H* N03       Office IS installed:  Display/Use of F1 = OV/400    *
     H*  04      SFLCLR                                               *
     H*  05      SFLEND                                               *
     H*  06      NON DISPLAY ON DOC ID/TYPE/DATE HEADINGS             *
     H*  07      GLOBAL - COMMAND KEY REQUESTED                       *
     H*  08/32   CPF5203-END OF ROLL                                  *
     H*  28      Highlight Remark Date if Completed Diary remark      *
     H*  31      GLOBAL - SCM0550-AS/400 Officevision is not installed*
     H*  34      GLOBAL - DATA ENTRY PROGRAM OPTION IS INSTALLED      *
     H*  36      SUMMARY CODE OPTION IS INSTALLED                     *
     H* N36      SUMMARY CODE OPTION IS NOT INSTALLED                 *
     H*  37      GLOBAL - Used on call to Data Entry Program (DEPGM)  *
     H*  54      SCM0501-VALUE ENTERED IS NOT DEFINED IN TABLE FILE   *
     H*  18      SCM0510-Invalid selection made                       *
     H*  60      GLOBAL - GENERAL CHAIN: PSL01, PCM01, PCG01          *EDIT PGM
     H*  91      SCM0601-INVALID REMARK SUMMARY CODE ENTERED.         *
     H*  92      Summary Code grouping requested from SCR02           *
     H*  97      GLOBAL - GENERAL CHAIN: PCM0A                        *
     H*  99      GLOBAL - IMAGE PRODUCT IS INSTALLED                  *
     H* SCR03:                                                        *
     H*  01      SFLDSP/SFLDSPCTL                                     *
     H*  02      IMAGE TYPE REMARK RECORD IS DISPLAYED                *
     H*  03       Office NOT installed: Nondisplay of F1=OV/400       *
     H* N03       Office IS installed:  Display/Use of F1 = OV/400    *
     H*  04      SFLCLR                                               *
     H*  05      SFLEND                                               *
     H*  06      NON DISPLAY ON DOC ID/TYPE/DATE HEADINGS             *
     H*  07      GLOBAL - COMMAND KEY REQUESTED                       *
     H*  08/32   CPF5203-END OF ROLL                                  *
     H*  09/10   SCM0536-INVALID VALUE DETECTED BY SPECIAL EDIT PROGRAM
     H*  09/N10  SCM0534-VALUE CANNOT BE BLANK OR ZERO - INVALID PER EDIT PGM
     H* N09/10   SCM0500-INVALID DATE ENTERED - NUMERIC (DATE) FIELDS *
     H* N09/10   SCM0535-INVALID VALUE ENTERED - AMOUNT FIELDS        *
     H* N09/N10  SCM0501-VALUE ENTERED IS NOT DEFINED IN TABLE FILE - TBL VALUE
     H*  11-20   HIGH INTENSITY - CUSTOM ID IN PATH PREVIOUSLY DISPLAYED
     H*  21-30   POSITIONAL INDICATOR FOR ALPHA FIELDS                *
     H*  28      Highlight Remark Date if Completed Diary remark      *
     H*  31      GLOBAL - SCM0550-AS/400 Officevision is not installed*
     H*  34      GLOBAL - DATA ENTRY PROGRAM OPTION IS INSTALLED      *
     H*  36      SUMMARY CODE OPTION IS INSTALLED                     *
     H* N36      SUMMARY CODE OPTION IS NOT INSTALLED                 *
     H*  37      GLOBAL - Used on call to Data Entry Program (DEPGM)  *
     H*  41-45   POSITIONAL INDICATOR FOR NUMERIC (DATE) FIELDS       *
     H*  46      SCM0501-VALUE ENTERED IS NOT DEFINED IN TABLE FILE   *
     H*  47      SCM0501-VALUE ENTERED IS NOT DEFINED IN TABLE FILE   *
     H*  48      SCM0501-VALUE ENTERED IS NOT DEFINED IN TABLE FILE   *
     H*  49-53   POSITIONAL INDICATOR FOR AMOUNT FIELDS               *
     H*  54      SCM0501-VALUE ENTERED NOT DEFINED IN TBL - INVALID CUSTOM ID
     H* N55      DISPLAY RECORD FORMAT A                              *
     H*  55      DISPLAY RECORD FORMAT B                              *
     H*  60      GLOBAL - GENERAL CHAIN: PSL01, PCM01, PCG01          *EDIT PGM
     H*  61-70   REVERSE IMAGE - CUSTOM ID IN PATH CURRENTLY DISPLAYED*
     H*  71-80   FIELD NOT DEFINED OR USER DOES NOT HAVE SEC AUTHORITY*- ALPHA
     H*  81-85   FIELD NOT DEFINED OR USER DOES NOT HAVE SEC AUTHORITY*- NUMERIC
     H*  86-90   FIELD NOT DEFINED OR USER DOES NOT HAVE SEC AUTHORITY*- AMOUNT
     H*  91      SCM0601-INVALID REMARK SUMMARY CODE ENTERED.         *
     H*  93      Summary Code grouping requested from SCR03           *
     H*  97      GLOBAL - GENERAL CHAIN: PCM0A                        *
     H* N98      ALLOW ENTRY,COL SEP,UNDERLINE                        *
     H*  98      PROTECT                                              *
     H*  99      GLOBAL - IMAGE PRODUCT IS INSTALLED                  *
     H* SCR06:                                                        *
     H*  03       Office NOT installed: Nondisplay of F1=OV/400       *
     H* N03       Office IS installed:  Display/Use of F1 = OV/400    *
     H*  07      GLOBAL - COMMAND KEY REQUESTED                       *
     H*  28      Highlight Remark Date if Completed Diary remark      *
     H*  31      GLOBAL - SCM0550-AS/400 Officevision is not installed*
     H*  32      CPF5203-END OF ROLL                                  *
     H*  33      SCM0515-NOT AUTHORIZED TO EDIT DOCUMENT              *
     H*  34      GLOBAL - DATA ENTRY PROGRAM OPTION IS INSTALLED      *
     H*  36      SUMMARY CODE OPTION IS INSTALLED                     *
     H* N36      SUMMARY CODE OPTION IS NOT INSTALLED                 *
     H*  37      GLOBAL - Used on call to Data Entry Program (DEPGM)  *
     H*  47    SCM0619-Must find alternative access to this data entry*
     H*                program.                                       *
     H*  48    SCM0620-Data entry program is not available at this    *
     H*                time.                                          *
     H*  50      SCM0513-NOT AUTHORIZED TO DELETE REMARK              *
     H*  51      SCM0516-DOCUMENT NOT FOUND IN SPECIFIED FOLDER       *
     H*  52      SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER         *
     H*  53      SCM0518-USER NOT ENROLLED IN OFFICE 400              *
     H*  54      SCM0510-INVALID VALUE KEYED                          *
     H*  55      SCM0511-DOCUMENT NAME DOES NOT EXIST IN USERS DEFAULT*FOLDER
     H*  57      SCM0514-NOT AUTHORIZED TO DISPLAY DOCUMENT           *
     H*  59      SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT  *
     H*  60      GLOBAL - GENERAL CHAIN: PSL01, PCM01, PCG01          *EDIT PGM
     H*  91      SCM0601-INVALID REMARK SUMMARY CODE ENTERED.         *
     H*  94      Summary Code grouping requested from SCR06           *
     H*  97      GLOBAL - GENERAL CHAIN: PCM0A                        *
     H*  99      GLOBAL - IMAGE PRODUCT IS INSTALLED                  *
     H*===============================================================*
     FSC010F    CF   E             WORKSTN usrOpn
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR02S:RRN2)
     F                                     SFILE(SCR03S:RRN3)
     F                                     SFILE(SCR06S:RRN6)
     F*** BEGIN CHG *** 04/25/00 **************************************
     F*****SDCML   IF  E           K        DISK                      A
     FSDCML     IF   E           K DISK
     F*** END CHG *** 04/25/00 ****************************************
     F******* BEGIN CHANGE *************************02/12/98 **********
     F*******SDCML1  IF  E           K        DISK
     FSDCMLM    IF   E           K DISK
     F******* BEGIN CHANGE *************************02/12/98 **********
     F                                     RENAME(PCM01:PCM02)
     FSDCMLD    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FSDCML3    IF   E           K DISK
     F                                     RENAME(PCM01:PCM04)
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM0A)
     FSMHDL     UF A E           K DISK
     FSMH1L     UF A E           K DISK
     F*
     F*** BEGIN ADD *** 06/14/04 **************************************
     FSDAIL     IF   E           K DISK
     F*** END   ADD *** 06/14/04 **************************************
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********SMCGL   IF  E           K        DISK
     F**********STTBL   IF  E           K        DISK
     F**********STTBL1  IF  E           K        DISK
     FSMCGL1    IF   E           K DISK
     FSTTBL4    IF   E           K DISK
     FSTTBL5    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     RENAME(PTB01:PTB02)
     FSMSLL     IF   E           K DISK
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********SMPPL   IF  E           K        DISK
     FSMPPL1    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     FSMPIL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FSMARL     UF   E           K DISK
     FSTSML     IF   E           K DISK
     F*** BEGIN DEL *** 04/25/00 **************************************
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM0I
     F*** END DEL *** 04/25/00 ****************************************
      *** BEGIN ADD *** 09/12/97 ***********************************************
     FSMUPL     IF   E           K DISK
      *** END   ADD *** 09/12/97 ***********************************************
      *** BEG ADD ***** 05/21/08 ***********************************************
      *** BEG CHG ***** 01/26/09 ***********************************************
     F********WMEML   IF  E           K        DISK
     FWMEML     IF   E           K DISK    USROPN
      *** END CHG ***** 01/26/09 ***********************************************
      *** END ADD ***** 05/21/08 ***********************************************
      *=========================================================================
      *  Array & Table Definitions
      *=========================================================================

     D*
     D*
     D*
     D*
     D*
     D*
     D*
     D RF              S              1    DIM(10)                              SCREEN PATH RFMTS
     D*
     D PTH             S              8    DIM(10)                              PATH ID'S FOR CONTIN
     D PHI             S             10    DIM(10)                              SCREEN PATH DSPATR(H
     D*
     D FL              S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES UA
     D FD              S              4    DIM(5) CTDATA PERRCD(5)              FIELD NAMES UD
     D FN              S              4    DIM(5) CTDATA PERRCD(5)              FIELD NAMES UN
     D FB              S              4    DIM(6) CTDATA PERRCD(6)              FIELD NAMES UB
     D*
      *** BEGIN CHG *** 11/19/99 ***********************************************
     D*************       CK      1  13 70               COMMAND KEYS
     D CK              S             70    DIM(14) CTDATA PERRCD(1)             COMMAND KEYS
      *** END   CHG *** 11/19/99 ***********************************************
     D CK1             S             70    DIM(7) CTDATA PERRCD(1)              COMMAND KEYS - SCR01
      *=========================================================================
      *  Data Structure and Constant Definitions
      *=========================================================================
     D*** BEGIN DEL *** 04/25/00 **************************************
     D********PCM0I
     D********      CMKEY                           PIKEY
     D********      CMSEQ#                          PISEQ#
     D*** END DEL *** 04/25/00 ****************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  UA                     1    100
     D                                     DIM(10)                              USER ASSIGN FLD
     D  SCUA01                 1     10
     D  SCUA02                11     20
     D  SCUA03                21     30
     D  SCUA04                31     40
     D  SCUA05                41     50
     D  SCUA06                51     60
     D  SCUA07                61     70
     D  SCUA08                71     80
     D  SCUA09                81     90
     D  SCUA10                91    100
     D*
     D                 DS
     D  UD                     1     30  0
     D                                     DIM(5)                               USER ASSIGN DATE
     D  SCUD01                 1      6  0
     D  SCUD02                 7     12  0
     D  SCUD03                13     18  0
     D  SCUD04                19     24  0
     D  SCUD05                25     30  0
     D*
     D                 DS
     D  UN                     1     45  2
     D                                     DIM(5)                               USER ASSIGN NUMB
     D  SCUN01                 1      9  2
     D  SCUN02                10     18  2
     D  SCUN03                19     27  2
     D  SCUN04                28     36  2
     D  SCUN05                37     45  2
     D*
     D                 DS
     D  UB                     1    210
     D                                     DIM(6)                               USER ASSIGN FLD 35A
     D  SCUB01                 1     35
     D  SCUB02                36     70
     D  SCUB03                71    105
     D  SCUB04               106    140
     D  SCUB05               141    175
     D  SCUB06               176    210
     D*
     D                 DS
     D  DA                     1    100
     D                                     DIM(10)                              USR ASGN FLD DSC
     D  SCDA01                 1     10
     D  SCDA02                11     20
     D  SCDA03                21     30
     D  SCDA04                31     40
     D  SCDA05                41     50
     D  SCDA06                51     60
     D  SCDA07                61     70
     D  SCDA08                71     80
     D  SCDA09                81     90
     D  SCDA10                91    100
     D*
     D                 DS
     D  DD                     1     50
     D                                     DIM(5)                               USR ASGN DAT DSC
     D  SCDD01                 1     10
     D  SCDD02                11     20
     D  SCDD03                21     30
     D  SCDD04                31     40
     D  SCDD05                41     50
     D*
     D                 DS
     D  DN                     1     50
     D                                     DIM(5)                               USR ASGN NUM DSC
     D  SCDN01                 1     10
     D  SCDN02                11     20
     D  SCDN03                21     30
     D  SCDN04                31     40
     D  SCDN05                41     50
     D*
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               USR ASGN 35A FLD DSC
     D  SCDB01                 1     10
     D  SCDB02                11     20
     D  SCDB03                21     30
     D  SCDB04                31     40
     D  SCDB05                41     50
     D  SCDB06                51     60
     D*
     D                 DS
     D  TA                     1     10
     D                                     DIM(10)                              TABLE BAKUP FLDS
     D  CGTA01                 1      1
     D  CGTA02                 2      2
     D  CGTA03                 3      3
     D  CGTA04                 4      4
     D  CGTA05                 5      5
     D  CGTA06                 6      6
     D  CGTA07                 7      7
     D  CGTA08                 8      8
     D  CGTA09                 9      9
     D  CGTA10                10     10
     D*
     D                 DS
     D  TD                     1      5
     D                                     DIM(5)                               TABLE BAKUP DATE
     D  CGTD01                 1      1
     D  CGTD02                 2      2
     D  CGTD03                 3      3
     D  CGTD04                 4      4
     D  CGTD05                 5      5
     D*
     D                 DS
     D  TN                     1      5
     D                                     DIM(5)                               TABLE BAKUP NUMB
     D  CGTN01                 1      1
     D  CGTN02                 2      2
     D  CGTN03                 3      3
     D  CGTN04                 4      4
     D  CGTN05                 5      5
     D*
     D                 DS
     D  TB                     1      6
     D                                     DIM(6)                               TABLE BAKUP 35A FLDS
     D  CGTB01                 1      1
     D  CGTB02                 2      2
     D  CGTB03                 3      3
     D  CGTB04                 4      4
     D  CGTB05                 5      5
     D  CGTB06                 6      6
     D*
     D                 DS
     D  MA                     1     10
     D                                     DIM(10)                              MANDATORY ENTRY
     D  CGMA01                 1      1
     D  CGMA02                 2      2
     D  CGMA03                 3      3
     D  CGMA04                 4      4
     D  CGMA05                 5      5
     D  CGMA06                 6      6
     D  CGMA07                 7      7
     D  CGMA08                 8      8
     D  CGMA09                 9      9
     D  CGMA10                10     10
     D*
     D                 DS
     D  MD                     1      5
     D                                     DIM(5)                               "
     D  CGMD01                 1      1
     D  CGMD02                 2      2
     D  CGMD03                 3      3
     D  CGMD04                 4      4
     D  CGMD05                 5      5
     D*
     D                 DS
     D  MN                     1      5
     D                                     DIM(5)                               "
     D  CGMN01                 1      1
     D  CGMN02                 2      2
     D  CGMN03                 3      3
     D  CGMN04                 4      4
     D  CGMN05                 5      5
     D*
     D                 DS
     D  MB                     1      6
     D                                     DIM(6)                               " FOR 35A FLDS
     D  CGMB01                 1      1
     D  CGMB02                 2      2
     D  CGMB03                 3      3
     D  CGMB04                 4      4
     D  CGMB05                 5      5
     D  CGMB06                 6      6
     D*
     D                 DS
     D  GA                     1    100
     D                                     DIM(10)                              SPECIAL EDIT PGM
     D  CGPA01                 1     10
     D  CGPA02                11     20
     D  CGPA03                21     30
     D  CGPA04                31     40
     D  CGPA05                41     50
     D  CGPA06                51     60
     D  CGPA07                61     70
     D  CGPA08                71     80
     D  CGPA09                81     90
     D  CGPA10                91    100
     D*
     D                 DS
     D  GD                     1     50
     D                                     DIM(5)                               "
     D  CGPD01                 1     10
     D  CGPD02                11     20
     D  CGPD03                21     30
     D  CGPD04                31     40
     D  CGPD05                41     50
     D*
     D                 DS
     D  GN                     1     50
     D                                     DIM(5)                               "
     D  CGPN01                 1     10
     D  CGPN02                11     20
     D  CGPN03                21     30
     D  CGPN04                31     40
     D  CGPN05                41     50
     D*
     D                 DS
     D  GB                     1     60
     D                                     DIM(6)                               " FOR 35A FLDS
     D  CGPB01                 1     10
     D  CGPB02                11     20
     D  CGPB03                21     30
     D  CGPB04                31     40
     D  CGPB05                41     50
     D  CGPB06                51     60
     D*
     D                 DS
     D  ED                     1      5
     D                                     DIM(5)                               EDIT DATE
     D  CGED01                 1      1
     D  CGED02                 2      2
     D  CGED03                 3      3
     D  CGED04                 4      4
     D  CGED05                 5      5
     D*
     D                 DS
     D  EN                     1      5
     D                                     DIM(5)                               EDIT NUMERIC
     D  CGEN01                 1      1
     D  CGEN02                 2      2
     D  CGEN03                 3      3
     D  CGEN04                 4      4
     D  CGEN05                 5      5
     D*
     D                 DS
     D  SA                     1     30  0
     D                                     DIM(10)                              SECURITY LEVEL
     D  CGSA01                 1      3  0
     D  CGSA02                 4      6  0
     D  CGSA03                 7      9  0
     D  CGSA04                10     12  0
     D  CGSA05                13     15  0
     D  CGSA06                16     18  0
     D  CGSA07                19     21  0
     D  CGSA08                22     24  0
     D  CGSA09                25     27  0
     D  CGSA10                28     30  0
     D*
     D                 DS
     D  SD                     1     15  0
     D                                     DIM(5)                               "
     D  CGSD01                 1      3  0
     D  CGSD02                 4      6  0
     D  CGSD03                 7      9  0
     D  CGSD04                10     12  0
     D  CGSD05                13     15  0
     D*
     D                 DS
     D  SN                     1     15  0
     D                                     DIM(5)                               "
     D  CGSN01                 1      3  0
     D  CGSN02                 4      6  0
     D  CGSN03                 7      9  0
     D  CGSN04                10     12  0
     D  CGSN05                13     15  0
     D*
     D                 DS
     D  SB                     1     18  0
     D                                     DIM(6)                               " FOR 35A FLDS
     D  CGSB01                 1      3  0
     D  CGSB02                 4      6  0
     D  CGSB03                 7      9  0
     D  CGSB04                10     12  0
     D  CGSB05                13     15  0
     D  CGSB06                16     18  0
     D*
     D                 DS
     D  PA                     1     20
     D                                     DIM(10)                              SCREEN PATH CSID'S
     D  SCPA01                 1      2
     D  SCPA02                 3      4
     D  SCPA03                 5      6
     D  SCPA04                 7      8
     D  SCPA05                 9     10
     D  SCPA06                11     12
     D  SCPA07                13     14
     D  SCPA08                15     16
     D  SCPA09                17     18
     D  SCPA10                19     20
     D*
     D                 DS
     D  CI                     1     20
     D                                     DIM(10)                              PATH FILE CSID'S
     D  PICI01                 1      2
     D  PICI02                 3      4
     D  PICI03                 5      6
     D  PICI04                 7      8
     D  PICI05                 9     10
     D  PICI06                11     12
     D  PICI07                13     14
     D  PICI08                15     16
     D  PICI09                17     18
     D  PICI10                19     20
     D*
     D                 DS
     D  FR                     1     10
     D                                     DIM(10)                              PATH FILE RFMTS
     D  PIRF01                 1      1
     D  PIRF02                 2      2
     D  PIRF03                 3      3
     D  PIRF04                 4      4
     D  PIRF05                 5      5
     D  PIRF06                 6      6
     D  PIRF07                 7      7
     D  PIRF08                 8      8
     D  PIRF09                 9      9
     D  PIRF10                10     10
     D******* BEGIN ADD ******* 05/07/07 ******************************
     D                 DS
     D  CMDCID                 1     12
     D  @DCID2                 1      2
     D******* END ADD ********* 05/07/07 ******************************
      *** BEG ADD ***** 05/21/08 **************************************
     D                 DS
     D  KKDS                   1     20
     D  KKCO#                  1      3  0
     D  KKFND                  4      6  0
     D  KKEMP#                 7     15  0
     D  KKDIV                 16     20  0
      *** END ADD ***** 05/21/08 **************************************
     DisUseGS          s              1n   inz
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D/copy sPrAtrium

     I*

      /Free
       isUseGS = not AtriumJob;

       Select;
       When not isUseGS;
          Open SC010F;
       Endsl;
      /End-free
     C***** DELETE THESE ***********************************
     C                   MOVE      Q2RECF        XXRECF           10            *
     C                   MOVEL     'QHLP    '    FILNAM           10            *
     C                   MOVEL     '*LIBL   '    LIBNAM           10            *
     C                   MOVE      XXSCR#        MBRNAM           10            *
      ******************************************************
      *=========================================================================
      *  *LIKE DEFN Definitions
      *=========================================================================
     C     *LIKE         DEFINE    TBFLD         XXFLD
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    HDKEY         QQKEY
     C     *LIKE         DEFINE    HDCSID        QQCSID
     C     *LIKE         DEFINE    HDPNAM        QQPNAM
     C     *LIKE         DEFINE    CMCDAT        XXDATE
     C     *LIKE         DEFINE    CMTRTM        XXTIME
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    TBVUA         XXVUA
     C     *LIKE         DEFINE    TBVUD         XXVUD
     C     *LIKE         DEFINE    TBVUN         XXVUN
     C     *LIKE         DEFINE    HDRFMT        HLRFMT
     C     *LIKE         DEFINE    HDRFMT        ##RFMT
     C     *LIKE         DEFINE    TBVUB         XXVUB
     C     *LIKE         DEFINE    DRAL02        OFCPGM
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    CMKEY         DEKEY
     C     *LIKE         DEFINE    CMCDAT        XXCDAT
     C     *LIKE         DEFINE    CMCDAT        XXDSDT
     C     *LIKE         DEFINE    CMCDAT        HHDT
     C     *LIKE         DEFINE    CMCDAT        @@CDAT
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     *LIKE         DEFINE    ARSEQ#        ##SEQ#
     C*** END ADD *** 04/25/00 ****************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    CGGRP         XXGRP
     C     *LIKE         DEFINE    XXGRP         ##GRP
     C***  END   ADD  ***  02/01/01  *********************************
      *** BEGIN ADD *** 11/19/99 ***********************************************
     C                   MOVE      ' '           XFVDTA            1
     C                   MOVEL     'SC010RVB'    VBDART            8
      *** END   ADD *** 11/19/99 ***********************************************
     C                   Z-ADD     0             C                 5 0
     C                   Z-ADD     0             C1                5 0
     C                   MOVE      'N'           XXDEWR            1            ADD 04/09/92
     C                   MOVE      *BLANKS       DERTCD            1            ADD 04/09/92
     C                   MOVE      *BLANKS       DECOM1          256            ADD 04/09/92
     C                   MOVE      *BLANKS       DECOM2           19            ADD 04/09/92
     C                   MOVE      *BLANKS       DEKEY                          ADD 04/09/92
      *=========================================================================
      *  KLIST Definitions
      *=========================================================================
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     AIKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*** END   ADD *** 06/14/04 **************************************
     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID DATA FILES (PHD01 & PH101)
     C*
     C     HDKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    SCCSID
     C                   KFLD                    SCRFMT
     C*
     C*   KEY LIST FOR THE REMARKS DETAIL RECORD
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   CMKYA     KLIST
     C********             KFLD           QQKEY
     C********             KFLD           ##SEQ#  50
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C     CMKYD         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    SCSEQ#
     C*
     C*   KEY LIST FOR THE REMARKS DETAIL RECORD
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C*
     C     CMKY1         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXDATE
     C                   KFLD                    XXTIME
     C                   KFLD                    XXSEQ#
     C*
     C     CMKY2         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    @@CDAT
     C*
     C     CMKY3         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    SCSRCD
     C*
     C     CMKY4         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    SCSRDC
     C*
     C*   KEY LIST FOR THE TABLE RECORD - RECORD FORMAT A
     C     TBKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    CGGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    SCCSID
     C                   KFLD                    FL(K)
     C                   KFLD                    XXVUA
     C                   KFLD                    XXVUD
     C                   KFLD                    XXVUN
     C*
     C*   KEY LIST FOR THE TABLE RECORD - RECORD FORMAT B
     C     TBKY2         KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    CGGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    SCCSID
     C                   KFLD                    FB(K)
     C                   KFLD                    XXVUB
     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     CGKY          KLIST
     C                   KFLD                    SCCSID
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     CGKY1         KLIST
     C                   KFLD                    SCCSID
     C                   KFLD                    XXGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    $$USER
     C*
     C*
     C*   KEY LIST FOR THE PROGRAM/PATH FILE
     C*
     C     PPKY          KLIST
     C                   KFLD                    QQPNAM
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     PPKY1         KLIST
     C                   KFLD                    QQPNAM
     C                   KFLD                    XXGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*
     C*   KEY LIST FOR THE PATH ID
     C*
     C     PIKY          KLIST
     C                   KFLD                    ##PID             8
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     PIKY1         KLIST
     C                   KFLD                    ##PID
     C                   KFLD                    XXGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*   KEY LIST FOR THE ADDITIONAL COMMENTS FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*   KEY LIST FOR THE REMARK SUMMARY CODE FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    SCSMCD
     C*
     C*   KEY LIST FOR THE REMARK TYPE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    CMRMCD
     C*
     C     CTKY1         KLIST
     C                   KFLD                    SCCODE
     C*
      *====== START ADD *** 1/05/98 ***** ISSIMG *****************
     C*
     C*   KEY LIST FOR DELETING REMARK DETAIL RECORD
     C*
     C     CMKY$         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    CMSEQ#
     C*
      *======== END ADD *** 1/05/98 ***** ISSIMG *****************
      *** BEGIN ADD *** 09/12/97 **************************************
      *
      *  User Parameter file
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *** END   ADD *** 09/12/97 **************************************
      *** BEG ADD ***** 05/21/08 **************************************
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *** END ADD ***** 05/21/08 **************************************
      *=========================================================================
      *  PLIST Definitions
      *=========================================================================
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY
     C                   PARM                    QQPNAM
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVEL     'SC'          XXSYK
     C*
      *** BEGIN ADD *** 11/19/99 ***********************************************
      *
      ** Visual DARTS call program
     C     VBDPL         PLIST
     C                   PARM                    QQKEY
     C                   PARM                    QQPNAM
      *** END   ADD *** 11/19/99 ***********************************************
      *=========================================================================
      *  Main
      *=========================================================================
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C*
      *** BEG ADD ***** 05/21/08 *************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C                   MOVE      X'27'         AT1
     C     XXCLT#        IFEQ      '093'
     C     QQPNAM        ANDEQ     'W0050R'
      *** BEG ADD ***** 01/26/09 ***********************************************
     C                   OPEN      WMEML
      *** END ADD ***** 01/26/09 ***********************************************
     C                   MOVEL(P)  QQKEY         KKDS
     C     EMKY          CHAIN     PEM01                              60
     C     EMVIP         IFEQ      'Y'
     C                   MOVE      X'21'         AT1
     C                   ELSE
     C                   MOVE      X'27'         AT1
     C                   ENDIF
      *** BEG ADD ***** 01/26/09 ***********************************************
     C                   CLOSE     WMEML
      *** END ADD ***** 01/26/09 ***********************************************
     C                   ENDIF
      *** END ADD ***** 05/21/08 *************************************
      *
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*   DISPLAY ALL FORMATS
     C*
      *------------------------*
      *** DISPLAY POINT
      *------------------------*
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
      **** BEGIN ADD **** 07/17/98 ************************************
      *
      * PROTECT DIARY TYPE
      *
     C     DRFL08        IFNE      'Y'
     C                   SETON                                        38
     C                   ENDIF                                                  DRFL08 IFNE 'Y'
      *
      * PROTECT PURGEABLE ITEM
      *
     C     DRFL09        IFNE      'Y'
     C                   SETON                                        39
     C                   ENDIF                                                  DRFL09 IFNE 'Y'
      **** END   ADD **** 07/17/98 ************************************
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C*** BEGIN ADD *** 02/07/00 ***********************************************
     C                   MOVE      *OFF          *IN56
     C*** END   ADD *** 02/07/00 ***********************************************
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   MOVE      '1'           *IN01
     C                   Z-ADD     3             SCLN                           ADD 07/18/96
     C                   Z-ADD     6             SCPS                           ADD 07/18/96
     C                   WRITE     SCR03
     C                   EXFMT     SCR03C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR06'
     C     Q2RECF        OREQ      'SCR06C'
     C     Q2RECF        OREQ      'SCR06S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR06
     C                   EXFMT     SCR06C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C/EJECT
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN07         IFEQ      '1'
     C****************************************
     C*   F1 = OV/400                        *
     C****************************************
     C     *INKA         IFEQ      '1'
     C*** BEG CHG *** 10/01/98 ****************************************
     C********** SLF03     IFEQ 'Y'
     C     SLF01         IFEQ      'Y'
     C*** END CHG *** 10/01/98 ****************************************
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      *BLANKS       OFCPGM
     C                   MOVEL     Q2RECF        HLRECF
     C*
     C* Get Office CLP Program Name from SDRPF/DRAL02
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRAL02        OFCPGM
     C                   END
     C*
     C                   CALL      OFCPGM
     C                   PARM                    QQRTCD
     C*
     C                   WRITE     SCR99
     C                   MOVEL     HLRECF        Q2RECF
     C                   END                                                    END SLF03=Y
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKA
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C     SLF03         IFEQ      'Y'
     C     Q2RECF        OREQ      'SCR04'                                      12/18/91 ADD
     C     Q2RECF        OREQ      'SCR05'                                      12/18/91 ADD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END SLF03=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKC
     C********************************************
     C*   F4 = REMARK SUMMARY CODE SEARCH        *
     C********************************************
     C     *INKD         IFEQ      '1'
     C********************************************
     C* SCR02 = REMARK SUMMARY CODE SEARCH       *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C                   CALL      'SC585R'
     C                   PARM                    SCSMCD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCSMCD        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END SCR02
     C********************************************
     C* SCR03 = REMARK SUMMARY CODE SEARCH       *
     C********************************************
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C                   CALL      'SC585R'
     C                   PARM                    SCSMCD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCSMCD        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END SCR03
     C********************************************
     C* SCR06 = REMARK SUMMARY CODE SEARCH       *
     C********************************************
     C     Q2RECF        IFEQ      'SCR06'
     C     Q2RECF        OREQ      'SCR06S'
     C     Q2RECF        OREQ      'SCR06C'
     C                   CALL      'SC585R'
     C                   PARM                    SCSMCD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCSMCD        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END SCR06
     C*
     C                   END                                                    END *INKF
     C****************************************
     C*   F5 = DISPLAY DOCUMENT              *
     C****************************************
     C     *INKE         IFEQ      '1'
     C     ADDDOC        TAG
     C     SLF05         IFEQ      'Y'
     C*
     C***********
     C*  SCR01  *  ADD DOC
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*
      * * * ADD DOCUMENT
     C*
     C                   MOVEA     '00000'       *IN(55)
     C                   MOVEA     '0000'        *IN(51)
     C                   MOVEA     '000000'      *IN(91)
     C*
     C     SLADC         IFNE      'Y'
     C                   MOVE      '1'           *IN57                          ADD NOT ALLOWED
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDCID        CABEQ     *BLANK        STRT                     91
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCDFLR        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SADDDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    DRFL06
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       XXUNCN          255
     C                   PARM      *BLANKS       XXFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN92
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN93
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN94
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '5'
     C                   MOVE      '1'           *IN95
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '6'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   END
     C*
     C*SCR02/SCR03 * * * INACTIVATE REMARKS
     C***********
     C*  SCR02  *
     C***********
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C*
     C                   MOVEL     Q2RECF        #6RECF           10
     C                   MOVE      *IN55         HLD55             1
     C                   MOVE      '0'           *IN55
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C     SORT          IFEQ      'DATE '
     C                   Z-ADD     9999999       @@CDAT
     C     SCSRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCSRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@CDAT
     C*
     C                   END
     C     CMKY2         SETLL     PCM02
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY3         SETLL     PCM03
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY4         SETLL     PCM04
     C                   ELSE
     C     CMKY1         SETLL     PCM01
     C                   END
     C                   END
     C                   END
     C*
     C*
     C* CHECK FOR VALID SUMMARY CODE
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C*
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      14            XXNUM
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '1'           *IN94
     C                   EXSR      LSMCD
     C                   ELSE                                                   ELS 1 *IN36
     C                   EXSR      LOAD6
     C                   END                                                    END 1 *IN36
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             #@$6
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C                   ELSE
     C***********
     C*  SCR03  *
     C***********
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR06'
     C     Q2RECF        OREQ      'SCR06S'
     C     Q2RECF        OREQ      'SCR06C'
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C                   MOVEL     Q2RECF        #6RECF           10
     C                   END
     C                   MOVE      *IN55         HLD55
     C                   MOVE      '0'           *IN55
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C     SORT          IFEQ      'DATE '
     C                   Z-ADD     9999999       @@CDAT
     C     SCSRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCSRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@CDAT
     C*
     C                   END
     C     CMKY2         SETLL     PCM02
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY3         SETLL     PCM03
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY4         SETLL     PCM04
     C                   ELSE
     C     CMKY1         SETLL     PCM01
     C                   END
     C                   END
     C                   END
     C*
     C* CHECK FOR VALID SUMMARY CODE
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C*
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      14            XXNUM
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '1'           *IN94
     C                   EXSR      LSMCD
     C                   ELSE                                                   ELS 1 *IN36
     C                   EXSR      LOAD6
     C                   END                                                    END 1 *IN36
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             #@$6
     C*
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C                   END
     C                   END
     C                   END                                                    END SLF05=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKE
     C****************************************
     C*  F6 = SEARCH DOCUMENT IN A FOLDER.  *
     C****************************************
     C     *INKF         IFEQ      '1'
     C     SLF06         IFEQ      'Y'
     C***********
     C*  SCR01  *  DISPLAY DOCUMENTS IN A FOLDER
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*
      * * * DSP DOCUMENTS
     C*
     C                   MOVEA     '00000'       *IN(55)                          SETOF ERR INDS
     C                   MOVEA     '0000'        *IN(51)
     C                   MOVEA     '000000'      *IN(91)
     C*
     C*
     C                   MOVEL     *BLANK        XXDCID                         RETURN DOC ID
     C                   MOVEL     SCDFLR        XXFOLD           80            DEFAULT FOLDER
     C                   MOVEL     *BLANK        QQRTCD            1            ERR RTN CODE
     C*
     C                   CALL      'SDSPFLR'                                    "DSPFLR" OFC API
     C                   PARM                    XXFOLD                         DFLT FOLDER
     C                   PARM                    XXDCID           12            RTN DOC ID SLCT
     C                   PARM                    QQRTCD                         ERR ID CODE
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN92
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN93
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '5'
     C                   MOVE      '1'           *IN95
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '6'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   END
     C*
     C     XXDCID        IFNE      *BLANK                                       VALID DOC ID
     C                   MOVEL     XXDCID        SCDCID                         USE IT, GOTO
     C                   GOTO      STRT                                         START
     C                   END
     C*
     C                   END
     C*
     C************
     C*  SCR02/3 *
     C************
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C                   MOVE      *BLANKS       ##PGM
     C                   CALL      'SRCHKPGM'
     C                   PARM      'SC015R'      ##PGM            10
     C                   PARM                    ##ERR             1
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           *IN58
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'SC015R'                             37
     C                   PARM                    QQKEY                          ADD 04/07/92
     C                   PARM                    QQRTCD
     C*** BEGIN ADD *** 09/22/97 ****************************************
     C                   PARM                    QQRMCD            5
     C                   PARM                    QPDATE            8
     C                   PARM                    QQUSER           10
     C*** BEGIN ADD *** 09/22/97 ****************************************
     C*
     C     *IN37         IFEQ      '1'
     C                   EXSR      ERROR
     C                   END
     C*
     C                   END                                                    END SCR02/3
     C*
     C*
     C                   END                                                    END SLF06=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C****************************************
     C*  F7 = CHANGE DEFAULT FOLDER ID.     *
     C****************************************
     C     *INKG         IFEQ      '1'
     C     SLF07         IFEQ      'Y'
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   MOVEL     SCDFLR        QQFOLD
     C*
     C                   CALL      'SC041R'
     C                   PARM                    QQFOLD           80
     C*
     C                   MOVEL     QQFOLD        SCDFLR
     C*
     C****** DO NOT EXECUTE THE FOLLOWING.  WE WILL NOT UPDATE THE FOLDER ST
     C   KG
     CANNKG              MOVEL     Q1USER        $$USER           10
     C   KG
     CANNKGSLKY          CHAIN     PSL01                              60
     C   KG
     CANNKG*IN60         IFEQ      '1'
     C   KG
     CANNKG              MOVEL     *BLANKS       $$USER           10
     C   KG
     CANNKGSLKY          CHAIN     PSL01                              60
     C   KG
     CANNKG*IN60         IFEQ      '1'
     C   KG
     CANNKG              MOVEL     *BLANK        Q2RECF
     C   KG
     CANNKG              MOVEL     'SCR04'       Q2RECF
     C   KG
     CANNKG              GOTO      STRT
     C                   END
     C                   END
     C*
     C   KG
     CANNKG              MOVEL     SLFOLD        SCDFLR
     C*
     C                   END                                                    END SLF07=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKG
     C****************************************
     C*  F8 = SCAN AN IMAGE (SCR01)         *
     C****************************************
     C     *INKH         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     SLF08         IFEQ      'Y'
     C*
     C************
     C*  SCR01   *
     C************
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXSR      EDTS01
     C*
     C*** LOAD PARAMETERS TO PASS TO SCAN PROGRAM
     C*
     C                   MOVEL     QQKEY         QSKEY
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        SCIQUE                             HIDDEN
     C                   MOVEL     *BLANK        SCIPTH                             HIDDEN
     C                   MOVE      *ZEROS        SCPIDX                             HIDDEN
     C                   MOVEL     *BLANK        SCAPID                             HIDDEN
     C                   MOVEL     *BLANK        SCIDOC                             HIDDEN
     C                   MOVEL     *BLANK        SCIMGF                             HIDDEN
     C                   MOVEL     *BLANK        SCQPTY                             HIDDEN
     C***8 CHANED NEXT LINE **** 1/5/98 ************
     C                   MOVEL     SCCODE        SCDCTP                             HIDDEN
     C********DEL 1/5/98   MOVEL*BLANK    SCDCTP               HIDDEN
     C                   MOVEL     *BLANK        SCFL05                             HIDDEN
     C*
     C*
     C                   MOVEL     *BLANKS       QSRTCD
     C*
      * ============ CHANGED THE ORDER OF THE DARTS RECORD WRITING ======*
      * ============ PROCESS BEFORE THE SCAN NOT AFTER ============ISSIMG*
      *  1/05/98  *** ADD ***
      *
     C                   MOVEL     '***IMAGE'    SCDCID
     C                   MOVE      '*** '        SCDCID                         SET FIELDS
     C                   MOVE      'Y'           SCIMGF
     C*
     C*** WRITE DARTS SCR01  RECORD ****
     C*** SCAN RECORD
     C                   EXSR      WCOM
     C*
      * ============ 1/05/98 CHANGED (ADDED) ABOVE ======================*
      *
     C*
     C*
     C                   CALL      'SCIMGS'                             60
     C                   PARM                    SCIDOC
     C                   PARM                    SCCOMM
     C                   PARM                    SCDUSR
     C                   PARM                    QSKEY            40
     C                   PARM                    SCIQUE
     C                   PARM                    SCAPID
     C                   PARM                    SCIPTH
     C                   PARM                    SCPIDX
     C                   PARM                    SCQPTY
     C                   PARM                    SCDCTP
     C**** ADDED NEXT 3 LINES  --- 1/05/98 -------------ISSIMG-
     C                   PARM                    CMSEQ#
     C                   PARM                    ZZFOLD           40
     C                   PARM                    ZZERRM           60
     C                   PARM                    QSRTCD            6
     C*
     C     QSRTCD        IFEQ      '     0'                                     VALID SCAN
     C     QSRTCD        OREQ      '000000'                                     VALID SCAN
     C     QSRTCD        OREQ      '      '                                     VALID SCAN
     C*
      * ============ DELETED CODE BELOW ===== 1/05/98  ==== ISSIMG ======*
     C*******              MOVEL'***IMAGE'SCDCID
     C*******              MOVE '*** '    SCDCID           SET FIELDS
     C*******              MOVE 'Y'       SCIMGF
      *
     C*** WRITE DARTS SCR01  RECORD ****
     C*** SCAN RECORD
     C******************** EXSR WCOM ********************************
     C*
      * ============ DELETED CODE ABOVE ===== 1/05/98  ==================*
      * ============ ADD CODE BELOW ========= 1/05/98  ==================*
      *
      *** UPDATE THE DARTS RECORD WITH THE IMAGE VALUES FROM SUCCESSFUL
      *** SCAN.
      *
     C                   EXSR      UPDSCN
      *
      * ======= END  ADD CODE ABOVE ========= 1/05/98  ==================*
     C*
     C*
     C****** WRITE DARTS SCR01 RECORD RECORD ****
     C*** GOTO NEXT SCREEN
     C*********DEL 1/5/98* EXSR WCOM
     C                   EXSR      RELOD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     $$RECF        Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*
     C*
      * =====START== ADDED CODE BELOW   ===== 8/29/97  === ISSIMG =======*
      *
     C                   EXSR      DLTSCN
      *
     C                   MOVEL     ZZERRM        ERRM
      *
      * ======END=== ADDED   CODE ABOVE ===== 1/05/98  ==================*
      *
     C**** ERROR OCCURED DISPLAY MESSAGE ON SCR01 ***
     C                   WRITE     SCR99
     C                   WRITE     SCR01
     C*
     C                   MOVE      '1'           *IN33
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*
     C                   END                                                    END SCR01
     C***********
     C*  SCR02/3/6*
     C***********
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR06'
     C     Q2RECF        OREQ      'SCR06S'
     C     Q2RECF        OREQ      'SCR06C'
     C*
     C                   MOVE      *BLANK        QFRTCD            2
     C                   CALL      'SCIMGF'                             60
     C                   PARM                    QFRTCD
     C*
     C     QFRTCD        IFNE      '00'
     C     QFRTCD        ANDNE     '  '
     C**** ERROR
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02/3
     C*
     C*
     C                   END                                                    END SLF08=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKH
     C*** BEG CHG *** 12/03/98 ****************************************
     C****************************************
     C*  F10 = USER PROFILE SEARCH           *
     C****************************************
     C     *INKJ         IFEQ      '1'
     C     HFLD          ANDEQ     'SCDUSR'
     C****************************************
     C***F9*=*USER PROFILE SEARCH           *
     C****************************************
     C********** *INKI     IFEQ '1'
     C*** END CHG *** 12/03/98 ****************************************
     C     SLF09         IFEQ      'Y'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END SLF09=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKI
     C****************************************
     C*  F10 = TABLE FILE SEARCH             *
     C****************************************
     C     *INKJ         IFEQ      '1'
     C     SLF10         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*** BEG CHG *** 12/03/98 ****************************************
     C     HFLD          IFEQ      'SCDEPT'
     C     HFLD          OREQ      'SCRMTY'
     C********** SCDEPT    IFEQ '? '
     C*** END CHG *** 12/03/98 ****************************************
     C***** BEGIN CHANGE ********* 03/27/98 ***************************
     C******               MOVEL*BLANKS   SCCODE
     C******
     C******               MOVE *BLANK    QQRTCD
     C******               CALL 'SC580R'
     C******               PARM           SCCODE
     C******               PARM           QQRTCD
     C******               MOVE *BLANK    Q2RECF
     C******               MOVEL'SCR01'   Q2RECF
     C******               MOVELSCCODE    SCDEPT
     C******               MOVE SCCODE    SCRMTY
     C******     CTKY1     CHAINPCT01                60
     C******     *IN60     IFEQ '0'
     C******               MOVELCTFL01    SCDITP
     C******               END
     C******               GOTO STRT
     C******               END                             SCCODE ='? '
     C******
     C********** SCRMTY    OREQ '? '
     C*** END CHG *** 12/03/98 ****************************************
     C******               MOVEL*BLANKS   SCCODE
     C******               MOVELSCDEPT    SCCODE
     C******               MOVE *BLANK    QQRTCD
     C******               CALL 'SC580R'
     C******               PARM           SCCODE
     C******               PARM           QQRTCD
     C*** BEG ADD *** 12/03/98 ***************************************
     C                   MOVEL(P)  SCDEPT        YYDEPT            5
     C*** END ADD *** 12/03/98 ***************************************
     C                   CALL      'SC305R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C*** BEG CHG *** 12/03/98 ***************************************
     C                   PARM      YYDEPT        QQRMCD            5
     C**********           PARM *BLANK    QQRMCD  5
     C*** END CHG *** 12/03/98 ***************************************
     C                   PARM      *BLANK        QQDESC           40
     C                   PARM      *BLANK        QQRTC             1
     C*** BEGIN ADD *** 02/10/99 **************************************
     C                   MOVEL(P)  QQDESC        SCCOMM
     C*** END ADD *** 02/10/99 ****************************************
     C*
     C     QQRTC         IFEQ      'A'
     C                   GOTO      STRT
     C                   ENDIF                                                  RTC = 'A'
     C*
     C     QQRTC         IFEQ      'B'
     C                   GOTO      STRT
     C                   ENDIF                                                  RTC = 'B'
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQRMCD        SCDEPT
     C                   MOVE      QQRMCD        SCRMTY
     C                   MOVE      QQRMCD        SCCODE
     C***** END   CHANGE ********* 03/27/98 ***************************
     C***  Begin Add  ***  07/24/08  **************************************
     C                   MOVEL     SCCODE        SVCODE
     C***  End   Add  ***  07/24/08  **************************************
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
     C***  Begin Add  ***  02/04/09  **************************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SVCODE
     C***  End   Add  ***  02/04/09  **************************************
     C                   END
     C                   ENDIF                                                  RTC = '*'
     C                   GOTO      STRT
     C                   END                                                    SCCODE ='? '
     C*
     C                   END                                                    END SCR01
     C***********
     C*  SCR03  *
     C***********
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C*
     C***
     C*  CUSTOM GROUP ID SEARCH
     C***
     C     SCCSID        IFEQ      '? '
     C*
     C                   MOVEL     SCCSID        ##CSID            2
     C                   MOVEL     SCRFMT        ##RFMT
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'SC520R'
     C                   PARM                    ##CSID
     C                   PARM                    ##RFMT
     C                   PARM                    QQRTCD            1
     C*
     C*** IF ALLOWED TO CHANGE CUSTOM ID, GET NEW VALUE BASED ON SELECTION
     C*** FROM THE SEARCH.  IF NOT ALLOWED TO CHANGE CSID, DON'T CHANGE IT AND
     C*** REDISPLAY SCREEN.
     C*
     C     SLCCIF        IFNE      'N'
     C                   MOVEL     ##CSID        SCCSID
     C                   MOVEL     ##RFMT        SCRFMT
     C                   ELSE
     C                   MOVE      HLCSID        SCCSID
     C                   MOVEL     HLRFMT        SCRFMT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     HLCSID        IFNE      ##CSID
     C     SLCCIF        IFNE      'N'
     C                   MOVE      '0'           ERRSEC            1
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      CGGETB
     C                   EXSR      H1GET
     C                   ELSE
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END
     C*
     C     ERRSEC        CABEQ     '1'           STRT                     54
     C                   MOVE      SCCSID        HLCSID            2
     C                   MOVE      SCRFMT        HLRFMT
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '1'           *IN54                          ERROR
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C                   GOTO      STRT
     C                   ELSE
     C*
     C***
     C*  10A ALPHA FIELD - TABLE SEARCH
     C***
     C                   DO        10            V                 5 0
     C                   MOVEL     UA(V)         XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVEL     SCCSID        ##KEY
     C                   MOVE      FL(V)         ##KEY
     C*
     C                   MOVE      *BLANKS       QQVUA            10
     C                   MOVE      *BLANKS       QQVUD             6
     C                   MOVE      *BLANKS       QQVUN             9
     C                   MOVE      *BLANKS       QQVUB            35
     C                   CALL      'SC552R'
     C                   PARM                    ##KEY             6
     C                   PARM                    QQVUA
     C                   PARM                    QQVUD
     C                   PARM                    QQVUN
     C                   PARM                    QQVUB
     C                   PARM                    QQRTCD            1
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM      XXGRP         ##GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   MOVEL     QQVUA         UA(V)
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END XX2 = ?
     C                   END                                                    END DO 10 V
     C*
     C***
     C*  35A ALPHA FIELD - TABLE SEARCH
     C***
     C                   DO        6             V                 5 0
     C                   MOVEL     UB(V)         XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVEL     SCCSID        ##KEY
     C                   MOVE      FB(V)         ##KEY
     C*
     C                   MOVE      *BLANKS       QQVUA            10
     C                   MOVE      *BLANKS       QQVUD             6
     C                   MOVE      *BLANKS       QQVUN             9
     C                   MOVE      *BLANKS       QQVUB            35
     C                   CALL      'SC552R'
     C                   PARM                    ##KEY             6
     C                   PARM                    QQVUA
     C                   PARM                    QQVUD
     C                   PARM                    QQVUN
     C                   PARM                    QQVUB
     C                   PARM                    QQRTCD            1
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM      XXGRP         ##GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   MOVEL     QQVUB         UB(V)
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END XX2 = ?
     C                   END                                                    END DO 10 V
     C*
     C***
     C*  6.0N NUMERIC FIELD - TABLE SEARCH
     C***
     C* Next line intentionally left open because of compilation of array
     C                   Z-ADD     1             V                 5 0
     C*
     C* Next line intentionally left open because of compilation of array
     C                   MOVE      FD(V)         ##KEY
     C*
     C***
     C*  9.2N NUMERIC FIELD - TABLE SEARCH
     C***
     C* Next line intentionally left open because of compilation of array
     C                   Z-ADD     1             V                 5 0
     C*
     C* Next line intentionally left open because of compilation of array
     C                   MOVE      FN(V)         ##KEY
     C*
     C                   END                                                    SCCSID ='? '
     C                   END                                                    END SCR03
     C*
     C                   END                                                    END SLF10=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKJ
     C****************************************
     C*   F11 = ADD REMARK                   *
     C****************************************
     C     *INKK         IFEQ      '1'
     C     SLF11         IFEQ      'Y'
     C                   Z-ADD     RRN2          HLRN              5 0
     C                   MOVEL     Q2RECF        $$RECF           10
     C                   MOVEL     *BLANK        SCCOMM
     C                   MOVEL     *BLANK        SCDCID
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        SCIQUE                             HIDDEN
     C                   MOVEL     *BLANK        SCIPTH                             HIDDEN
     C                   MOVE      *ZEROS        SCPIDX                             HIDDEN
     C                   MOVEL     *BLANK        SCAPID                             HIDDEN
     C                   MOVEL     *BLANK        SCIDOC                             HIDDEN
     C                   MOVEL     *BLANK        SCIMGF                             HIDDEN
     C                   MOVEL     *BLANK        SCQPTY                             HIDDEN
     C                   MOVEL     *BLANK        SCDCTP                             HIDDEN
     C                   MOVEL     *BLANK        SCFL05                             HIDDEN
     C*
     C                   MOVEL     'N'           SCARMK                             SCREEN FLAG
     C*
     C*** DEFAULT TO CURRENT USER FOR DIARY USER, BUT USER MAY CHANGE TO ANOTHER
     C                   MOVEL     Q1USER        SCDUSR
     C*
     C*** CHAIN TO SECURITY FILE FOR DEFAULTS AND AUTHORIZATION
     C                   MOVEL     Q1USER        $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   Initialize flag for checking to see if Data Entry Program
     C*     has been called once already.
     C*
     C                   MOVE      'N'           XXDEFL            1
     C*
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     *BLANKS       SCDEPT
     C                   MOVEL     *BLANKS       SCRMTY
     C     SLRMCD        IFNE      *BLANKS
     C                   MOVEL     SLRMCD        SCDEPT
     C                   MOVE      SLRMCD        SCRMTY
     C                   MOVEL     SLRMCD        SCCODE
     C                   ELSE
     C                   MOVEL     SLCODE        SCDEPT
     C                   MOVEL     SLCODE        SCCODE
     C                   END
     C***  Begin Add  ***  07/24/08  **************************************
     C     *LIKE         DEFINE    SCCODE        SVCODE
     C                   MOVEL     SCCODE        SVCODE
     C***  End   Add  ***  07/24/08  **************************************
     C*                                                    03/20/92
     C*    INITIALIZE 'PURGEABLE ITEM' FIELD TO BLANK      03/20/92
     C*                                                    03/20/92
     C                   MOVE      *BLANK        SCPGFL                         03/20/92
     C                   MOVE      ' '           SCDITP
     C*** IF ENTERED, REMARK TYPE MUST BE VALID
     C     SCCODE        IFNE      *BLANKS
     C     SCCODE        CHAIN     STCTL1                             60
     C*
     C******** BEGIN ADD ***** 02/26/98 *******************************
     C     *IN60         IFEQ      *OFF
     C******** END   ADD ***** 02/26/98 *******************************
     C*** BEG DEL *** 09/15/98 ****************************************
     C**** DETERMINE IF REMARK TYPE IS DIARY OR NOT *****
     C********** CTFL01    IFEQ 'Y'
     C**********           MOVE 'Y'       SCDITP
     C**********           ELSE
     C**********           MOVE 'N'       SCDITP
     C**********           ENDIF
     C******** BEGIN ADD ***** 02/26/98 *******************************
     C**********           ELSE
     C**********           MOVE 'N'       SCDITP
     C**********           ENDIF
     C******** END   ADD ***** 02/26/98 *******************************
     C*** END DEL *** 09/15/98 ****************************************
     C*
     C*** BEG ADD *** 09/15/98 ****************************************
     C*
     C     SLRMCD        IFNE      *BLANKS
     C                   MOVE      CTFL01        SCDITP
     C                   ELSE
     C                   MOVE      ' '           SCDITP
     C                   ENDIF                                                   SLRMCD NE *BLANKS
     C***  Begin Add  ***  02/04/09  **************************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SVCODE
     C***  End   Add  ***  02/04/09  **************************************
     C                   ENDIF                                                   *IN60 = *0FF
     C*** END ADD *** 09/15/98 ****************************************
     C*
     C                   END                                                     SCCODE NE *BLANKS
     C*
     C     SLCCTF        IFNE      'Y'
     C                   MOVE      '1'           *IN98                          PROTECT REMARK TYPE
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     *ZERO         SCSEQ#
     C                   MOVEL     *ZERO         SCSQ#
     C*
     C* If OV/400 is installed
     C     *IN03         IFEQ      '0'
     C                   Z-ADD     2             C1                                         RK TYPE
     C* If OV/400 is NOT installed
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   END
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C                   MOVE      *IN55         HLD55
     C                   MOVE      '0'           *IN55
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SLF11=Y
     C                   END                                                    END *INKK
     C****************************************
     C*   F12 = PREVIOUS SCREEN(CANCEL)      *
     C****************************************
     C     *INKL         IFEQ      '1'
     C     SLF12         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*   RETRIEVE VALUE OF *IN55 ON PRIOR SCREEN & CLEAR HOLD FIELD
     C                   MOVE      HLD55         *IN55
     C                   MOVE      '0'           HLD55
     C                   MOVEL     $$RECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C***********
     C*  SCR03  * *** PRIOR PATH CUSTOMID
     C***********
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C*
     C*
     C*** DO PATH HERE
     C     #P            IFGT      1
     C     #P            SUB       1             NXTP              5 0
     C     PA(NXTP)      IFNE      *BLANK
     C     #P            ADD       10            #H                5 0
     C     #P            ADD       60            #R                5 0
     C                   MOVE      '0'           *IN(#R)
     C                   SUB       1             #P
     C                   SUB       1             #R
     C                   MOVE      '1'           *IN(#R)
     C                   MOVE      PA(#P)        SCCSID
     C                   MOVE      PA(#P)        HLCSID
     C                   MOVE      RF(#P)        SCRFMT
     C                   MOVE      RF(#P)        HLRFMT
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      H1GET
     C                   EXSR      CGGETB
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END                                                    END EQ 'B'
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C*** SAVE CURRENT PATH AND INDICATORS
     C*
     C     @P            ADD       1             $P                5 0
     C                   MOVEL     PIPATH        PTH($P)
     C*
     C                   MOVE      *BLANK        #10FLD
     C                   MOVEA     *IN(11)       #10FLD           10
     C                   MOVEA     #10FLD        PHI($P)
     C*
     C     @P            IFGT      0
     C                   MOVEL     PTH(@P)       ##PID
     C                   EXSR      PIGET
     C                   MOVEL     PA(10)        SCCSID
     C                   MOVEL     PA(10)        HLCSID
     C                   MOVEL     RF(10)        SCRFMT
     C                   MOVEL     RF(10)        HLRFMT
     C                   MOVE      '0'           *IN61
     C                   MOVE      '1'           *IN70
     C*
     C                   MOVE      *BLANK        #10FLD
     C                   MOVEA     PHI(@P)       #10FLD           10
     C                   MOVEA     #10FLD        *IN(11)
     C*
     C                   SUB       1             @P                5 0
     C                   Z-ADD     10            #P
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      H1GET
     C                   EXSR      CGGETB
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END                                                    END EQ 'B'
     C*
     C                   END
     C*
     C***** IF EQ 1  OR LT 1, WE NEED TO GET THE PRIOR PATH RCD AND CONTINUE.
     C*** CLEARING THE CURRENT ONE  IN  PTH ARRAY.     IF NEEDED.
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C***********
     C*  SCR06  *
     C***********
     C     Q2RECF        IFEQ      'SCR06'
     C     Q2RECF        OREQ      'SCR06S'
     C     Q2RECF        OREQ      'SCR06C'
     C*
     C* * * RE-LOAD SUBFILES   * * *
     C*
     C*   RETRIEVE VALUE OF *IN55 ON PRIOR SCREEN & CLEAR HOLD FIELD
     C                   MOVE      HLD55         *IN55
     C                   MOVE      '0'           HLD55
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C     SORT          IFEQ      'DATE '
     C                   Z-ADD     9999999       @@CDAT
     C     SCSRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCSRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@CDAT
     C*
     C                   END
     C     CMKY2         SETLL     PCM02
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY3         SETLL     PCM03
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY4         SETLL     PCM04
     C                   ELSE
     C     CMKY1         SETLL     PCM01
     C                   END
     C                   END
     C                   END
     C*
     C*
     C* CHECK FOR VALID SUMMARY CODE
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C*
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      4             XXNUM
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '0'           *IN94
     C                   EXSR      LSMCD
     C                   ELSE                                                   ELS 1 *IN36
     C                   EXSR      LOAD2
     C                   END                                                    END 1 *IN36
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             #@$6
     C*
     C                   MOVEL     #6RECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END SLF12=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C****************************************
     C*   F13 = DISPLAY USER ASS. FLDS       *
     C****************************************
     C     *INKM         IFEQ      '1'
     C     SLF13         IFEQ      'Y'
     C***********
     C*  SCR02  *
     C***********
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C                   Z-ADD     #@$2          #@$3
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   MOVEA     '00000'       *IN(21)
     C                   MOVEA     '00000'       *IN(26)
     C     C             IFEQ      5
     C                   Z-ADD     6             C
     C                   END                                                    END EQ 5
     C     C             IFEQ      7
     C                   Z-ADD     8             C
     C                   END                                                    END EQ 7
     C     C             IFEQ      9
     C                   Z-ADD     10            C
     C                   END                                                    END EQ 9
     C     C             IFEQ      11
     C                   Z-ADD     12            C
     C                   END                                                    END EQ 11
     C                   ELSE                                                   END SCR02
     C***********
     C*  SCR03  *
     C***********
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C                   Z-ADD     #@$3          #@$2
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    6
     C                   Z-ADD     5             C
      *
     C     C             WHENEQ    8
     C                   Z-ADD     7             C
      *
     C     C             WHENEQ    10
     C                   Z-ADD     9             C
      *
     C     C             WHENEQ    12
     C                   Z-ADD     11            C
      *
     C                   ENDSL
      *** END   CHG *** 11/19/99 ***********************************************
     C                   END                                                    END SCR03
     C                   END                                                    END SCR02
     C*
     C                   MOVEL     CK(C)         SCCMDL                         ADD 04/06/92
     C*
     C                   END                                                    END SLF13=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKM
     C****************************************
     C*   F14 = Summary Code Sequence        *
     C****************************************
     C     *INKN         IFEQ      '1'
     C     SLF14         IFEQ      'Y'
     C*
     C* CHECK FOR VALID SUMMARY CODE
     C     *IN36         IFEQ      '1'
     C     SCSMCD        IFNE      *BLANKS
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       XXRECF
     C                   MOVEL     Q2RECF        XXRECF
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C***********
     C*  SCR02  *
     C***********
     C     XXRECF        IFEQ      'SCR02'
     C     XXRECF        OREQ      'SCR02S'
     C     XXRECF        OREQ      'SCR02C'
     C                   Z-ADD     4             XXNUM
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '0'           *IN94
     C                   END                                                    END SCR02
     C***********
     C*  SCR03  *
     C***********
     C     XXRECF        IFEQ      'SCR03'
     C     XXRECF        OREQ      'SCR03S'
     C     XXRECF        OREQ      'SCR03C'
     C                   Z-ADD     2             XXNUM
     C                   MOVE      '0'           *IN92
     C                   MOVE      '1'           *IN93
     C                   MOVE      '0'           *IN94
     C                   END                                                    END SCR03
     C***********
     C*  SCR06  *
     C***********
     C     XXRECF        IFEQ      'SCR06'
     C     XXRECF        OREQ      'SCR06S'
     C     XXRECF        OREQ      'SCR06C'
     C                   Z-ADD     14            XXNUM
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '1'           *IN94
     C                   END                                                    END SCR06
     C*
     C     SORT          IFEQ      'DATE '
     C                   Z-ADD     9999999       @@CDAT
     C     SCSRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCSRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@CDAT
     C*
     C                   END
     C     CMKY2         SETLL     PCM02
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY3         SETLL     PCM03
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY4         SETLL     PCM04
     C                   ELSE
     C     CMKY1         SETLL     PCM01
     C                   END
     C                   END
     C                   END
     C*
     C                   EXSR      LSMCD
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     XXRECF        Q2RECF
     C*
     C                   END                                                    *IN60 = 1
     C                   END                                                    SCSMCD
     C                   END                                                    *IN36 = 1
     C*
     C                   END                                                    END SLF14=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKN
     C****************************************
     C*   F15 = DATE SEQUENCE                *
     C****************************************
     C     *INKP         IFEQ      '1'
     C     SLF15         IFEQ      'Y'
     C*
     C                   MOVEL     'DATE '       SORT              5
     C                   MOVEL     Q2RECF        ##RECF           10
     C*
     C                   Z-ADD     9999999       @@CDAT
     C     SCSRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCSRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@CDAT
     C*
     C                   END
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C     CMKY2         SETLL     PCM02                                  60
     C*
     C* CHECK FOR SUMMARY CODE OPTION
     C*
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C                   Z-ADD     4             XXNUM
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '0'           *IN94
     C*
     C                   EXSR      LSMCD
     C                   ELSE
     C                   EXSR      LOAD2
     C                   END
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             #@$6
     C*
     C                   MOVEL     ##RECF        Q2RECF
     C*
     C                   END                                                    END SLF15=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKP
     C****************************************
     C*   F16 = TYPE SEQUENCE                *
     C****************************************
     C     *INKQ         IFEQ      '1'
     C     SLF16         IFEQ      'Y'
     C*
     C                   MOVEL     'TYPE '       SORT              5
     C                   MOVEL     Q2RECF        ##RECF           10
     C*
     C*** SET LIMITS BASED ON VALUES ENTERED ON SCREEN
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C     CMKY3         SETLL     PCM03                                  60
     C*
     C* CHECK FOR SUMMARY CODE OPTION
     C*
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C                   Z-ADD     4             XXNUM
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '0'           *IN94
     C*
     C                   EXSR      LSMCD
     C                   ELSE
     C                   EXSR      LOAD2
     C                   END
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             #@$6
     C*
     C                   MOVEL     ##RECF        Q2RECF
     C*
     C                   END                                                    END SLF16=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKQ
     C****************************************
     C*   F17 = DOC ID SEQUENCE              *
     C****************************************
     C     *INKR         IFEQ      '1'
     C     SLF17         IFEQ      'Y'
     C*
     C                   MOVEL     'DOCID'       SORT              5
     C                   MOVEL     Q2RECF        ##RECF           10
     C*
     C*** SET LIMITS BASED ON VALUES ENTERED ON SCREEN
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C     CMKY4         SETLL     PCM04                                  60
     C*
     C* CHECK FOR SUMMARY CODE OPTION
     C*
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C                   Z-ADD     4             XXNUM
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '0'           *IN94
     C*
     C                   EXSR      LSMCD
     C                   ELSE
     C                   EXSR      LOAD2
     C                   END
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             #@$6
     C*
     C                   MOVEL     ##RECF        Q2RECF
     C                   END                                                    END SLF17=Y
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKR
     C****************************************
     C*   F18 = SEQUENCE ENTERED             *
     C****************************************
     C     *INKS         IFEQ      '1'
     C     SLF18         IFEQ      'Y'
     C*
     C*
     C                   MOVEL     'ENTER'       SORT              5
     C                   MOVEL     Q2RECF        ##RECF           10
     C*
     C*** SET LIMITS BASED ON VALUES ENTERED ON SCREEN
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             RRN3
     C                   Z-ADD     0             RRN6
     C*
     C     CMKY1         SETLL     PCM01                                  60
     C*
     C* CHECK FOR SUMMARY CODE OPTION
     C*
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C                   Z-ADD     4             XXNUM
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '0'           *IN94
     C*
     C                   EXSR      LSMCD
     C                   ELSE
     C                   EXSR      LOAD2
     C                   END
     C*
     C                   Z-ADD     1             #@$2
     C                   Z-ADD     1             #@$3
     C                   Z-ADD     1             #@$6
     C*
     C                   MOVEL     ##RECF        Q2RECF
     C                   END                                                    END SLF18=Y
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKS
     C****************************************
     C*   F19 = PRINT REMARKS                *
     C****************************************
     C     *INKT         IFEQ      '1'
     C     SLF19         IFEQ      'Y'
     C*
     C                   MOVEL     QQKEY         QRKEY            40
     C                   MOVE      *BLANKS       QRUSER           10
     C                   MOVE      *ZEROS        QRFRDT            8
     C                   MOVE      *ZEROS        QRTODT            8
     C                   MOVE      *BLANKS       QRSMCD            4
     C                   MOVE      *BLANKS       QRRMTY            5
     C                   MOVE      *BLANKS       QRDCID           12
     C                   MOVE      'A'           QRTYPE            1
     C                   MOVE      *BLANKS       QRDITY            1
     C                   MOVE      'KY'          QRSORT            2
     C                   MOVE      *BLANKS       QRRTCD            1
     C                   CALL      'SC711R'
     C                   PARM                    QRKEY
     C                   PARM                    QRUSER
     C                   PARM                    QRFRDT
     C                   PARM                    QRTODT
     C                   PARM                    QRSMCD
     C                   PARM                    QRRMTY
     C                   PARM                    QRDCID
     C                   PARM                    QRTYPE
     C                   PARM                    QRDITY
     C                   PARM                    QRSORT
     C                   PARM                    QRRTCD
     C*
     C                   END                                                    END SLF19=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKT
     C**********************************************************
     C*  F20= SET DEFAULT FOLDER AND DOCUMENT ID - AUTO CREATE
     C**********************************************************
     C     *INKU         IFEQ      '1'
     C*
     C* VALIDATE REMARK TYPE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C*
     C* REMARK TYPE MUST BE ENTERED
     C     SCCODE        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ELSE
     C     SCCODE        CHAIN     STCTL1                             24
     C     *IN24         CABEQ     '1'           STRT
     C     CTTYPE        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANKS       SCDFLR
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANKS       QQCODE
     C                   MOVEL     CTTYPE        QQCODE
     C                   MOVE      *BLANKS       QQDFLR
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVE      *BLANKS       QQERR
     C                   CALL      'SCEXITC'
     C                   PARM                    QQKEY
     C                   PARM                    QQPNAM
     C                   PARM                    QQCODE            2
     C                   PARM                    QQDFLR           40
     C                   PARM                    QQDCID           12
     C                   PARM                    QQERR             1
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    DRFL06
     C     QQERR         IFNE      *BLANKS
     C     QQERR         IFEQ      '1'
     C* INCOMING PROGRAM DOES NOT ALLOW AUTO CREATE FUNCTION
     C                   MOVE      '1'           *IN25
     C                   END                                                    QQERR=1
     C     QQERR         IFEQ      '2'
     C* NOT ABLE TO AUTO BUILD FOLDER
     C                   MOVE      '1'           *IN22
     C                   END                                                    QQERR=2
     C     QQERR         IFEQ      '3'
     C* NOT ABLE TO AUTO BUILD DOCUMENT ID
     C                   MOVE      '1'           *IN23
     C                   END                                                    QQERR=3
     C                   GOTO      STRT
     C                   END                                                    QQERR<>" "
     C                   MOVE      *BLANKS       SCDFLR
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     QQDFLR        SCDFLR
     C                   MOVEL     QQDCID        SCDCID
     C                   GOTO      ADDDOC
     C                   END                                                    END *INKU
     C****************************************
     C*   F23=SECURE REMARKS  BY LEVEL       *
     C****************************************
     C     *INKX         IFEQ      *ON
     C     SLF23         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C*  Remark UNSECURED to SECURED
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN56         IFEQ      '0'
     C*** BEGIN DEL *** 02/07/00 ***********************************************
     C**********           MOVEL*BLANKS   SCCODE
     C**********           MOVELSCDEPT    SCCODE
     C**********           MOVE SCRMTY    SCCODE
     C********** CTKY1     CHAINPCT01                59    +DEFAULT SECLVL
     C********** *IN59     IFEQ '1'
     C********** CTDSLV    OREQ *ZEROS
     C**********           MOVE SLSECL    SCSECL
     C**********           ELSE
     C**********           MOVE CTDSLV    SCSECL
     C**********           END
     C**********           MOVE SCSECL    ##SECL  30
     C*** END   DEL *** 02/07/00 ***********************************************
     C                   MOVE      *ON           *IN56
     C                   MOVE      'Y'           ##SBYL            1
     C*** DISPLAY "UNSECURE REMARKS", IF "SECURE REMARKS" WAS DISPLAYED
     C     C1            IFEQ      6
     C                   Z-ADD     7             C1
     C                   END
     C*
     C                   ELSE
     C*  Remark SECURED to UNSECURED
     C                   MOVE      *OFF          *IN56
     C*** BEGIN DEL *** 02/07/00 ***********************************************
     C**********           MOVE *ZEROS    SCSECL
     C*** END   DEL *** 02/07/00 ***********************************************
     C                   MOVE      'N'           ##SBYL            1
     C*** DISPLAY "SECURE REMARKS", IF "UNSECURE REMARKS" WAS DISPLAYED
     C     C1            IFEQ      7
     C                   Z-ADD     6             C1
     C                   END
     C                   END
     C                   MOVEL     CK1(C1)       SCCML1                         ADD 04/06/92
     C*** BEGIN ADD *** 02/07/00 ***********************************************
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C     CTDSLV        OREQ      *ZEROS
     C                   MOVE      SLSECL        SCSECL
     C                   ELSE
     C                   MOVE      CTDSLV        SCSECL
     C                   END
     C                   MOVE      SCSECL        ##SECL            3 0
     C*** END   ADD *** 02/07/00 ***********************************************
     C*
     C                   END
     C*
     C                   END                                                    END SLF23=Y
      *** BEGIN ADD *** 11/19/99 ***********************************************
      ** F23=Visual DARTS
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
      *
     C     XFVDTA        IFEQ      'Y'
     C                   CALL      VBDART        VBDPL
     C                   ENDIF                                                  XFVDTA = *ON
      *
     C                   ENDIF                                                  Q2RECF = SCR02/SCR03
      *** END   ADD *** 11/19/99 ***********************************************
     C                   GOTO      STRT
     C                   END                                                    END *INKX
     C****************************************
     C*   F24=More Keys                      *
     C****************************************
     C     *INKY         IFEQ      '1'
     C     SLF24         IFEQ      'Y'
     C***********
     C*  SCR01  *
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C* Office NOT Installed, Image Installed, Remark Secured
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     *IN56         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
     C     C1            WHENEQ    1
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     5             C1
      *
     C     C1            WHENEQ    5
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C                   Z-ADD     1             C1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image Installed, Remark NOT Secured
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     *IN56         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    1
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     5             C1
      *
     C     C1            WHENEQ    5
     C                   Z-ADD     6             C1
      *
     C     C1            WHENEQ    6
     C                   Z-ADD     1             C1
      *
     C                   ENDSL
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image NOT Installed, Remark Secured
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '0'
     C     *IN56         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    1
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     4             C1
      *
     C     C1            WHENEQ    4
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C                   Z-ADD     1             C1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image NOT Installed, Remark NOT Secured
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '0'
     C     *IN56         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    1
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     4             C1
      *
     C     C1            WHENEQ    4
     C                   Z-ADD     6             C1
      *
     C     C1            WHENEQ    6
     C                   Z-ADD     1             C1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image Installed, Remark Secured
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '1'
     C     *IN56         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    2
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     5             C1
      *
     C     C1            WHENEQ    5
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C                   Z-ADD     2             C1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image Installed, Remark NOT Secured
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '1'
     C     *IN56         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    2
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     5             C1
      *
     C     C1            WHENEQ    5
     C                   Z-ADD     6             C1
      *
     C     C1            WHENEQ    6
     C                   Z-ADD     2             C1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image NOT Installed, Remark Secured
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '0'
     C     *IN56         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    2
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     4             C1
      *
     C     C1            WHENEQ    4
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C                   Z-ADD     2             C1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image NOT Installed, Remark NOT Secured
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '0'
     C     *IN56         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    2
     C                   Z-ADD     3             C1
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     4             C1
      *
     C     C1            WHENEQ    4
     C                   Z-ADD     6             C1
      *
     C     C1            WHENEQ    6
     C                   Z-ADD     2             C1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  03=99=56=*ON
      *** END   CHG *** 11/19/99 ***********************************************
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C                   END                                                    END SCR01
     C*****************
     C*  SCR02        *
     C*****************
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C*
     C* Office NOT Installed, Image Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    1
     C                   Z-ADD     7             C
      *
     C     C             WHENEQ    7
     C                   Z-ADD     9             C
      *
     C     C             WHENEQ    9
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     1             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     1             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image NOT Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    1
     C                   Z-ADD     5             C
      *
     C     C             WHENEQ    5
     C                   Z-ADD     9             C
      *
     C     C             WHENEQ    9
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     1             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     1             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    2
     C                   Z-ADD     7             C
      *
     C     C             WHENEQ    7
     C                   Z-ADD     9             C
      *
     C     C             WHENEQ    9
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     2             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     2             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image NOT Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    2
     C                   Z-ADD     5             C
      *
     C     C             WHENEQ    5
     C                   Z-ADD     9             C
      *
     C     C             WHENEQ    9
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     2             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     2             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image Installed, Sum Cd Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    3
     C                   Z-ADD     7             C
      *
     C     C             WHENEQ    7
     C                   Z-ADD     11            C
      *
     C     C             WHENEQ    11
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     3             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     3             C
      *
     C                   ENDSL
      *** END   CHG *** 11/19/99 ***********************************************
      *
     C                   ENDIF
     C*
     C* Office NOT Installed, Image NOT Installed, Sum Cd Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    3
     C                   Z-ADD     5             C
      *
     C     C             WHENEQ    5
     C                   Z-ADD     11            C
      *
     C     C             WHENEQ    11
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     3             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     3             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image Installed, Sum Cd Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    4
     C                   Z-ADD     7             C
      *
     C     C             WHENEQ    7
     C                   Z-ADD     11            C
      *
     C     C             WHENEQ    11
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     4             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     4             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image NOT Installed, Sum Cd Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    4
     C                   Z-ADD     5             C
      *
     C     C             WHENEQ    5
     C                   Z-ADD     11            C
      *
     C     C             WHENEQ    11
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     4             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     4             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C                   ENDIF                                                  END SCR02
     C*****************
     C*  SCR03        *
     C*****************
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C*
     C* Office NOT Installed, Image Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    1
     C                   Z-ADD     8             C
      *
     C     C             WHENEQ    8
     C                   Z-ADD     10            C
      *
     C     C             WHENEQ    10
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     1             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     1             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image NOT Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    1
     C                   Z-ADD     6             C
      *
     C     C             WHENEQ    6
     C                   Z-ADD     10            C
      *
     C     C             WHENEQ    10
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     1             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     1             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    2
     C                   Z-ADD     8             C
      *
     C     C             WHENEQ    8
     C                   Z-ADD     10            C
      *
     C     C             WHENEQ    10
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     2             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     2             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image NOT Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    2
     C                   Z-ADD     6             C
      *
     C     C             WHENEQ    6
     C                   Z-ADD     10            C
      *
     C     C             WHENEQ    10
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     2             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     2             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image Installed, Sum Cd Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    3
     C                   Z-ADD     8             C
      *
     C     C             WHENEQ    8
     C                   Z-ADD     12            C
      *
     C     C             WHENEQ    12
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     3             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     3             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image NOT Installed, Sum Cd Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    4
     C                   Z-ADD     6             C
      *
     C     C             WHENEQ    6
     C                   Z-ADD     12            C
      *
     C     C             WHENEQ    12
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     4             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     4             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office Installed, Image Installed, Sum Cd Installed
     C     *IN03         IFEQ      '0'
     C     *IN99         ANDEQ     '1'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    4
     C                   Z-ADD     8             C
      *
     C     C             WHENEQ    8
     C                   Z-ADD     12            C
      *
     C     C             WHENEQ    12
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     4             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     4             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C* Office NOT Installed, Image NOT Installed, Sum Cd Installed
     C     *IN03         IFEQ      '1'
     C     *IN99         ANDEQ     '0'
     C     *IN36         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C             WHENEQ    3
     C                   Z-ADD     6             C
      *
     C     C             WHENEQ    6
     C                   Z-ADD     12            C
      *
     C     C             WHENEQ    12
     C                   Z-ADD     13            C
      *
     C     C             WHENEQ    13
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     14            C
     C                   ELSE
     C                   Z-ADD     3             C
     C                   ENDIF
      *
     C     C             WHENEQ    14
     C                   Z-ADD     3             C
      *
     C                   ENDSL
      *
     C                   ENDIF
      *** END   CHG *** 11/19/99 ***********************************************
     C                   END                                                    END SCR03
     C**
     C                   MOVEL     CK(C)         SCCMDL
     C                   END                                                    END SLF24=Y
     C                   GOTO      STRT
     C                   END                                                    END *INKY
     C****************************************
     C*  *IN08 - ROLL UP                     *
     C****************************************
     C     *IN08         IFEQ      '1'                                          ROLLUP
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C*
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C*
     C* CHECK FOR SUMMARY CODE OPTION
     C*
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C                   Z-ADD     4             XXNUM
     C                   MOVE      '1'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '0'           *IN94
     C*
     C                   EXSR      LSMCD
     C                   ELSE
     C*
     C                   EXSR      LOAD2
     C*
     C                   END                                                    *IN36 = 1
     C*
     C                   MOVEL     *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   END                                                    SCR02
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C*
     C*
     C* CHECK FOR SUMMARY CODE OPTION
     C*
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C                   Z-ADD     2             XXNUM
     C                   MOVE      '0'           *IN92
     C                   MOVE      '1'           *IN93
     C                   MOVE      '0'           *IN94
     C*
     C                   EXSR      LSMCD
     C                   ELSE
     C*
     C                   EXSR      LOAD3
     C*
     C                   END                                                    *IN36 = 1
     C*
     C                   MOVEL     *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C                   END                                                    SCR03
     C*
     C     Q2RECF        IFEQ      'SCR06'
     C     Q2RECF        OREQ      'SCR06S'
     C     Q2RECF        OREQ      'SCR06C'
     C*
     C* CHECK FOR SUMMARY CODE OPTION
     C*
     C     *IN36         IFEQ      '1'
     C     SCSMCD        ANDNE     *BLANKS
     C                   Z-ADD     14            XXNUM
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           *IN93
     C                   MOVE      '1'           *IN94
     C*
     C                   EXSR      LSMCD
     C                   ELSE
     C*
     C                   EXSR      LOAD6
     C*
     C                   END                                                    *IN36 = 1
     C*
     C                   MOVEL     *BLANK        Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C*
     C                   END                                                    SCR06
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END 1 *IN08 ROLLUP
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11/CMDKEYS
     C*
     C*
      *****************************************************************
      *   GOTO THE APPROPRIATE SCREEN
      *****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02S'      SCR02T
     C     Q2RECF        CABEQ     'SCR02C'      SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR03S'      SCR03T
     C     Q2RECF        CABEQ     'SCR03C'      SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C     Q2RECF        CABEQ     'SCR06'       SCR06T
     C     Q2RECF        CABEQ     'SCR06S'      SCR06T
     C     Q2RECF        CABEQ     'SCR06C'      SCR06T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C                   EXSR      EDTS01                                        EDIT SCR01 FIELDS
     C*
     C*** WRITE REMARK RECORD
     C                   EXSR      WCOM
     C*
     C*
     C*** RELOAD SCREEN
     C                   EXSR      RELOD
     C*
     C*   RETRIEVE VALUE OF *IN55 ON PRIOR SCREEN & CLEAR HOLD FIELD
     C                   MOVE      HLD55         *IN55
     C                   MOVE      '0'           HLD55
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     $$RECF        Q2RECF
     C**
     C                   GOTO      STRT
     C*
      *------------------------*
     C     SCR02T        TAG
      *------------------------*
     C*
     C*
     C                   MOVE      RRN2          SRRN2             5 0          SAVE RRN (AVOID ROLL
     C*                                                       ERRORS LATER.)
     C                   MOVE      '0'           ERR
     C*
     C                   READC     SCR02S                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C**** Option 7=Display/Add Additional Remarks
     C     SCSEL         IFEQ      '7'
     C                   MOVEL     QQKEY         #7KEY            40
     C                   MOVE      SCSEQ#        #7SEQ#            5 0
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SC014R'
     C                   PARM                    #7KEY
     C                   PARM                    #7SEQ#
     C                   PARM                    QQPNAM                         12/18/91 ADD
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C*
     C                   UPDATE    SCR02S
     C                   END
     C*
     C**** Verify a valid option was selected
     C     SCSEL         IFNE      ' '
     C     SCSEL         ANDNE     '7'
     C                   MOVE      '1'           *IN18
     C                   MOVE      RRN2          XRRN2
     C*
     C                   UPDATE    SCR02S
     C                   MOVE      '1'           ERR               1
     C                   END
     C*
     C     NXT2          TAG
     C*
     C                   READC     SCR02S                                 60
     C                   END
     C*
     C                   MOVE      SRRN2         RRN2                           RE-APPLY SAVED RRN
     C*
     C     ERR           IFEQ      '1'
     C                   MOVE      XRRN2         #@$2
     C                   GOTO      STRT
     C                   END
     C*
     C*  Set at last record selected
     C*
     C                   Z-ADD     XRRN2         #@$2
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
      *------------------------*
     C     SCR03T        TAG
      *------------------------*
     C*
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** CGKY      CHAINPCG01                60
     C                   CLEAR                   CGGRP
     C     CGKY1         CHAIN     PCG01                              60
     C   60CGKY          CHAIN     PCG01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C     *IN60         CABEQ     '1'           STRT                     54    ERROR
     C*
     C     HLCSID        IFNE      SCCSID
     C     SLCCIF        IFNE      'N'
     C                   MOVE      '0'           ERRSEC            1
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      CGGETB
     C                   EXSR      H1GET
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END                                                    END EQ 'B'
     C*
     C     ERRSEC        CABEQ     '1'           STRT                     54
     C                   MOVE      SCCSID        HLCSID            2
     C                   MOVE      SCRFMT        HLRFMT
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '1'           *IN54                          ERROR
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C* ===================================================================
     C*
     C**** EDIT FOR RECORD FORMAT 'B'
     C     SCRFMT        IFEQ      'B'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 35A ALPHA FIELDS
     C*
     C                   Z-ADD     20            Z                 5 0
     C                   Z-ADD     70            Y                 5 0
     C                   Z-ADD     0             K                 5 0
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C*
     C                   DO        6             X
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C*
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF 35A ALPHA FIELDS ***
     C     MB(X)         IFEQ      'Y'
     C     UB(X)         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR 35A ALPHA FIELDS ***
     C     GB(X)         IFNE      *BLANKS
     C     UB(X)         ANDNE     *BLANKS
     C                   MOVE      ' '           GBRTCD
     C                   MOVE      '0'           *IN60
     C                   CALL      GB(X)                                60
     C                   PARM      *BLANKS       GBUA             10
     C                   PARM      *ZEROS        GBUD              6 0
     C                   PARM      *ZEROS        GBUN              9 2
     C                   PARM      UB(X)         GBUB             35
     C                   PARM                    GBRTCD            1
     C*
     C     *IN60         IFEQ      '0'
     C     GBRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END                                                    END GBRTCD EQ '1'
     C                   END                                                    END 0 *IN60
     C                   END                                                    END GB,X NE *BLANKS
     C*
     C*
     C*** TABLE BACKUP VALUES FOR ALPHA FIELDS ***
     C     TB(X)         IFEQ      'Y'
     C     UB(X)         ANDNE     *BLANK
     C*
     C                   MOVE      *BLANKS       XXVUA
     C                   MOVE      *ZEROS        XXVUD
     C                   MOVE      *ZEROS        XXVUN
     C                   MOVE      UB(X)         XXVUB
     C*
     C     TBKY2         CHAIN     PTB02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(Z)
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END                                                    END 0 *IN,Y
     C*
     C                   END                                                    END D0 6 X
     C*
     C*
     C*
     C* ===================================================================
     C**** "DEFAULT" EDIT
     C                   ELSE                                                   ELSE EQ 'B'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 10A ALPHA FIELDS
     C*
     C                   Z-ADD     20            Z                 5 0
     C                   Z-ADD     70            Y                 5 0
     C                   Z-ADD     0             K                 5 0
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C                   DO        10            X
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C*
     C                   MOVEL     *OFF          *IN(Z)
     C*
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF ALPHA FIELDS ***
     C     MA(X)         IFEQ      'Y'
     C     UA(X)         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR ALPHA FIELDS ***
     C     GA(X)         IFNE      *BLANKS
     C     UA(X)         ANDNE     *BLANKS
     C                   MOVE      ' '           GARTCD
     C                   MOVE      '0'           *IN60
     C                   CALL      GA(X)                                60
     C                   PARM      UA(X)         GAUA             10
     C                   PARM      *ZEROS        GAUD              6 0
     C                   PARM      *ZEROS        GAUN              9 2
     C                   PARM      *BLANKS       GAUB             35
     C                   PARM                    GARTCD            1
     C*
     C     *IN60         IFEQ      '0'
     C     GARTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*
     C*
     C*** TABLE BACKUP VALUES FOR ALPHA FIELDS ***
     C     TA(X)         IFEQ      'Y'
     C     UA(X)         ANDNE     *BLANK
     C*
     C                   MOVE      UA(X)         XXVUA
     C                   MOVE      *ZEROS        XXVUD
     C                   MOVE      *ZEROS        XXVUN
     C                   MOVE      *BLANKS       XXVUB
     C*
     C     TBKY          CHAIN     PTB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(Z)
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 6.0 NUMERIC FIELDS
     C*
     C                   Z-ADD     40            Z                 5 0
     C                   Z-ADD     80            Y                 5 0
     C                   Z-ADD     10            K                 5 0
     C*
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C*
     C                   DO        05            X
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C*
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF NUMERIC (DATE) FIELDS ***
     C     MD(X)         IFEQ      'Y'
     C     UD(X)         ANDEQ     *ZERO
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C*
     C*** DATE EDIT OF NUMERIC (DATE) FIELDS ******
     C     ED(X)         IFEQ      'Y'
     C     UD(X)         ANDNE     *ZEROS
     C*
     C*
     C                   Z-ADD     UD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*                    MOVELSRDTO     UD,X
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR NUMERIC (DATE) FIELDS
     C     GD(X)         IFNE      *BLANKS
     C     UD(X)         ANDNE     *ZEROS
     C                   MOVE      ' '           GARTCD
     C                   MOVE      '0'           *IN60
     C                   CALL      GD(X)                                60
     C                   PARM      *BLANKS       GAUA             10
     C                   PARM      UD(X)         GAUD              6 0
     C                   PARM      *ZEROS        GAUN              9 2
     C                   PARM      *BLANKS       GAUB             35
     C                   PARM                    GARTCD            1
     C*
     C     *IN60         IFEQ      '0'
     C     GARTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*
     C*
     C     TD(X)         IFEQ      'Y'
     C     UD(X)         ANDNE     *ZEROS
     C*
     C                   MOVE      *BLANKS       XXVUA
     C                   MOVE      UD(X)         XXVUD
     C                   MOVE      *ZEROS        XXVUN
     C                   MOVE      *BLANKS       XXVUB
     C*
     C     TBKY          CHAIN     PTB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(Z)
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 9.2 NUMERIC FIELDS
     C*
     C                   Z-ADD     48            Z                 5 0
     C                   Z-ADD     85            Y                 5 0
     C                   Z-ADD     15            K                 5 0
     C*
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C*
     C                   DO        05            X
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C*
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF AMOUNT FIELDS ***
     C     MN(X)         IFEQ      'Y'
     C     UN(X)         ANDEQ     *ZERO
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C*
     C*** POSITIVE EDIT OF AMOUNT FIELDS ******
     C     EN(X)         IFEQ      '1'
     C     UN(X)         ANDLT     0
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C*
     C*** NEGATIVE EDIT OF AMOUNT FIELDS ******
     C     EN(X)         IFEQ      '2'
     C     UN(X)         ANDGT     0
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR AMOUNT FIELDS
     C     GN(X)         IFNE      *BLANKS
     C     UN(X)         ANDNE     *ZEROS
     C                   MOVE      ' '           GARTCD
     C                   MOVE      '0'           *IN60
     C                   CALL      GN(X)                                60
     C                   PARM      *BLANKS       GAUA             10
     C                   PARM      *ZEROS        GAUD              6 0
     C                   PARM      UN(X)         GAUN              9 2
     C                   PARM      *BLANKS       GAUB             35
     C                   PARM                    GARTCD            1
     C*
     C     *IN60         IFEQ      '0'
     C     GARTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*
     C*
     C     TN(X)         IFEQ      'Y'
     C     UN(X)         ANDNE     *ZEROS
     C*
     C                   MOVE      *BLANKS       XXVUA
     C                   MOVE      *ZEROS        XXVUD
     C                   MOVE      UN(X)         XXVUN
     C                   MOVE      *BLANKS       XXVUB
     C*
     C     TBKY          CHAIN     PTB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(Z)
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END                                                    END EQ 'B'
     C*
     C*** ALL EDITS PASSED, WRITE DATA BASE RECORDS.
     C*
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      WRITEB
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      WRITEA
     C                   END                                                    END EQ 'B'
     C*
     C*** DO PATH HERE
     C     #P            IFLT      10
     C     #P            ADD       1             NXTP              5 0
     C     PA(NXTP)      IFNE      *BLANK
     C     #P            ADD       10            #H                5 0
     C     #P            ADD       60            #R                5 0
     C                   MOVE      '1'           *IN(#H)
     C                   MOVE      '0'           *IN(#R)
     C                   ADD       1             #P
     C                   ADD       1             #R
     C                   MOVE      '1'           *IN(#R)
     C                   MOVE      PA(#P)        SCCSID
     C                   MOVE      PA(#P)        HLCSID
     C                   MOVE      RF(#P)        SCRFMT
     C                   MOVE      RF(#P)        HLRFMT
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      H1GET
     C                   EXSR      CGGETB
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END                                                    END EQ 'B'
     C*
     C                   ELSE
     C     #P            ADD       10            #H                5 0
     C                   MOVE      '1'           *IN(#H)
     C                   END
     C                   ELSE
     C     #P            ADD       10            #H                5 0
     C                   MOVE      '1'           *IN(#H)
     C*
     C*** TEST IF A NEXT PATH IS TO BE DISPLAYED.
     C     @P            ADD       2             @P2               5 0
     C     @P2           IFLE      10
     C     PTH(@P2)      IFEQ      *BLANK
     C     PINXPI        CABEQ     *BLANK        STRT
     C                   END
     C                   ELSE
     C     PINXPI        CABEQ     *BLANK        STRT
     C                   END
     C**** COPIED
     C     @P            IFLT      10
     C                   ADD       1             @P
     C                   MOVEL     PIPATH        PTH(@P)
     C*
     C                   MOVE      *BLANK        #10FLD
     C                   MOVEA     *IN(11)       #10FLD           10
     C                   MOVEA     #10FLD        PHI(@P)
     C***
     C                   END
     C     @P2           IFLE      10
     C     PTH(@P2)      IFNE      *BLANK
     C                   MOVEL     PTH(@P2)      ##PID
     C                   EXSR      PIGET
     C*
     C                   MOVE      *BLANK        #10FLD
     C                   MOVEA     PHI(@P2)      #10FLD           10
     C                   MOVEA     #10FLD        *IN(11)
     C*
     C                   ELSE
     C                   MOVEL     PINXPI        ##PID
     C                   EXSR      PIGET
     C                   END
     C                   END
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      H1GET
     C                   EXSR      CGGETB
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END                                                    END EQ 'B'
     C*
     C************
     C*TEMPORARY TAG TO BYPASS CODE
     C                   GOTO      TMPTAG
     C*
     C***********
     C     @P            IFLT      10
     C                   ADD       1             @P
     C                   MOVEL     PIPATH        PTH(@P)
     C*
     C                   MOVE      *BLANK        #10FLD
     C                   MOVEA     *IN(11)       #10FLD           10
     C                   MOVEA     #10FLD        PHI(@P)
     C*
     C*
     C*** IF WE ARE RETURNING TO AN ALREADY DISPLAYED PATH ID, SHOW *IN11
     C*** STUFF (WHICH WERE ALREADY ENTERED.)
     C*
     C     @P            IFLE      10
     C     @P            ADD       1             @P2               5 0
     C     PTH(@P2)      IFNE      *BLANK
     C                   MOVEL     PTH(@P2)      ##PID
     C                   EXSR      PIGET
     C*
     C                   MOVE      *BLANK        #10FLD
     C                   MOVEA     PHI(@P2)      #10FLD           10
     C                   MOVEA     #10FLD        *IN(11)
     C*
     C                   ELSE
     C                   MOVEL     PINXPI        ##PID
     C                   EXSR      PIGET
     C                   END
     C                   ELSE
     C                   MOVEL     PINXPI        ##PID
     C                   EXSR      PIGET
     C                   END
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      H1GET
     C                   EXSR      CGGETB
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END                                                    END EQ 'B'
     C*
     C                   END
     C*
     C*TEMPORARY TAG TO BYPASS CODE
     C     TMPTAG        TAG
     C*
     C***** IF EQ 10 OR GT 10 , WE NEED TO GET THE NEXT PATH RCD AND CONTINUE.
     C*** SAVING THE PRIOR ONE.     IN  PTH ARRAY.     IF NEEDED.
     C                   END
     C*
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C*
     C                   GOTO      STRT
     C*
      *------------------------*
     C     SCR04T        TAG
      *------------------------*
     C*
     C                   GOTO      STRT
     C*
     C*
      *------------------------*
     C     SCR05T        TAG
      *------------------------*
     C*
     C                   GOTO      STRT
     C*
     C*
      *------------------------*
     C     SCR06T        TAG
      *------------------------*
     C*
     C* * * EDIT FIELDS
     C*
     C                   MOVE      RRN6          SRRN6             5 0          SAVE RRN (AVOID ROLL
     C*                                                       ERRORS LATER.)
     C                   MOVE      '0'           ERR
     C*
     C                   READC     SCR06S                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     SCSEL         IFEQ      '4'
     C                   MOVEA     '000000'      *IN(50)
     C                   MOVE      '0'           *IN57
     C                   MOVE      '0'           *IN59
     C                   MOVE      '0'           *IN33
     C*
     C     SLDELC        IFNE      'Y'
     C**DEL******SCIMGF    OREQ 'Y'          *** DEL 1/5/99 ***
     C                   MOVE      '1'           *IN50                          SCM0513
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C*
     C* * * DELETE/INACTIVATE RECORDS
     C*
     C     CMKYD         CHAIN     PCM0A                              97
     C     *IN97         IFEQ      '0'
     C     ARKY          CHAIN     PAR01                              97
     C     *IN97         DOWEQ     '0'
     C                   MOVE      'I'           ARSTAT
     C*** Begin Change 11/06/02 **************************************
     C****************     Z-ADD*ZERO     ARSEQ#
     C*** End   Change 11/06/02 **************************************
     C                   UPDATE    PAR01
     C     ARKY          READE     PAR01                                  97
     C                   END
     C*
     C*
     C*** Begin Change 04/02/02 **************************************
     C*****************    Z-ADD*ZERO     CMSEQ#
     C*** End   Change 04/02/02 **************************************
     C                   MOVE      'I'           CMSTAT
     C                   UPDATE    PCM0A
     C                   END
     C                   MOVE      *BLANKS       SCCO45
     C                   MOVEL     '*DELETED'    SCCO45
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   ELSE
     C*
     C     SCSEL         IFEQ      '5'
     C*
     C* PERFORM THIS CODE ONLY IF DATA ENTRY PROGRAM OPTION EXISTS IN
     C*    SDRPF (DRFL03 = 'Y')
     C*   NOTE: DERTCD = '1' is 'View')
     C*
     C     *IN34         IFEQ      '1'
     C     CMKYD         CHAIN     PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     CMFL01        IFEQ      'Y'
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C***  BEGIN CHG  ***  10/08/99  *********************************
     C****BEG*ADD *** 11/23/98 *****************************
     C**********           MOVELCMDCID    ##PGM
     C********** ##PGM     IFEQ 'F0269C1'
     C**********           MOVEL'F0269R'  ##PGM     P
     C**********           ENDIF
     C**********
     C*****BEGIN ADD  ***  05/27/99  *********************************
     C********** ##PGM     IFEQ 'W0070C'
     C**********           MOVEL'W0070R'  ##PGM     P
     C**********           ENDIF                           ##PGM=W0070C
     C*****END** ADD  ***  05/27/99  *********************************
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      *OFF          ADDINF            1
     C     ##DEIF        IFEQ      'Y'
     C     AIKY          CHAIN     PAI01                              99
     C                   ELSE
     C                   SETON                                        99
     C                   ENDIF
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *ON           ADDINF
     C                   SELECT
     C     AIDPGM        WHENEQ    'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C     AIDPGM        WHENEQ    'W0070C'
     C                   MOVEL(P)  'W0070R'      ##PGM
     C     AIDPGM        WHENEQ    'W0050C01'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   OTHER
     C                   MOVEL     AIDPGM        ##PGM
     C                   ENDSL
     C                   ELSE
     C*** END   ADD *** 06/14/04 **************************************
     C                   SELECT
     C     CMDCID        WHENEQ    'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C     CMDCID        WHENEQ    'W0070C'
     C                   MOVEL(P)  'W0070R'      ##PGM
     C     CMDCID        WHENEQ    'W0050C01'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   OTHER
     C                   MOVEL     CMDCID        ##PGM
     C                   ENDSL
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C***  END   CHG  ***  10/08/99  *********************************
     C*
     C*** END ADD *** 11/23/98 *****************************
     C***********          MOVE *BLANKS   ##PGM            DEL 11/23/98
     C                   CALL      'SRCHKPGM'
     C*** BEG CHG *** 11/23/98 *****************************
     C***********          PARM CMDCID    ##PGM  10
     C                   PARM                    ##PGM            10
     C*** END CHG *** 11/23/98 *****************************
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           *IN47
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       DEPGM             8
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ADDINF        IFEQ      *OFF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMDCID        DEPGM
     C                   MOVEL     CMFOLD        DEKEY
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   ELSE
     C                   MOVEL     AIDPGM        DEPGM
     C                   MOVEL     AIDKEY        DEKEY
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMCOMM        DECOM1
     C                   MOVE      CMCOMM        DECOM2
     C                   MOVE      '1'           DERTCD
     C*
     C                   CALL      DEPGM                                37
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
     C*
     C     *IN37         IFEQ      '1'
     C                   EXSR      ERROR
     C                   GOTO      NXT6
     C                   END
     C*
     C                   EXCEPT    RCM0A
     C*
     C     DERTCD        IFEQ      'A'
     C     DERTCD        OREQ      'B'
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   ELSE
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C* CMFL01 = 'N', DATA ENTRY PROGRAM ID DOES NOT EXIST FOR THIS
     C*    REMARK TYPE; THEREFORE, PROCESS DOC AS USUAL
     C*
     C                   ELSE
     C                   EXCEPT    RCM0A
     C                   GOTO      DDOC
     C                   END                                                    CMFL01 = 'Y'
     C*
     C                   END                                                    *IN41 = '0'
     C*
     C                   END                                                    *IN34 = '1'
     C*
     C     DDOC          TAG
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C                   MOVEA     '000000'      *IN(50)
     C                   MOVE      '0'           *IN57
     C                   MOVE      '0'           *IN59
     C                   MOVE      '0'           *IN33
     C*
     C     SLDDC         IFNE      'Y'
     C                   MOVE      '1'           *IN57
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C     SCIMGF        IFNE      'Y'                                          IF NOT AN IMAGE TYPE
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     QQKEY         XXKY             40
     C                   MOVEL     *BLANK        QQRTCD            1
     C*
     C     SCIMGF        IFEQ      'F'
     C                   MOVEL     SCIMGF        $MFL06            1
     C                   ELSE
     C                   MOVEL     DRFL06        $MFL06
     C                   ENDIF
     C***** 5/08/04 - ADD START ***********************************
     C*
     C     DRFL13        IFNE      *BLANKS
     C     SCIMGF        ANDEQ     'O'
     C*** 4/17/07 *** ADD ****************************************
     C                   MOVEL     DRFL13        QQRTCD
     C*** 4/17/07 *** END ADD ************************************
     C                   CALL      'SDSPDC2'
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C                   PARM                    QQRTCD
     C                   PARM                    $MFL06
     C*** CHECK FOR ERRORS HERE......
     C***
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN51
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN52
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN53
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C*
     C*** BYPASS OTHER ...
     C                   GOTO      SKPDC
     C*
     C                   ENDIF
     C*
     C***** 5/08/04  -  ADD END ************************************
     C                   CALL      'SDSPDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    $MFL06
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       XXUNCN          255
     C                   PARM      *BLANKS       XXFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN51
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN52
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN53
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C****** ADD START 05/08/04 ***************************************
     C     SKPDC         TAG
     C****** END ADD   05/08/04 ****************************************
     C*
     C*
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   ELSE
     C*
     C*** START CHANGES *** 1/05/98 ***************** ISSIMG **********
     C                   MOVEL     SCIDOC        QVDCTB          120
     C                   MOVEL     Q1USER        QVUSER           10
     C                   MOVEL     Q1JOB         QVTRID           10
     C                   MOVEL     *BLANK        QVRTCD            6
     C*
     C                   MOVEL     SCIDOC        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     QQKEY         XXKY             40
     C                   MOVEL     *BLANK        QVRTCD
     C*
     C*
     C                   CALL      'SCIMGV'                             60
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM                    QVRTCD
     C*** END CHANGES 1/05/98 **************************
     C*
     C*
     C     QVRTCD        IFNE      '     0'
     C     QVRTCD        ANDNE     '000000'
     C     QVRTCD        ANDNE     '      '
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   END
     C*
     C                   END
     C*
     C*
     C     SCSEL         IFEQ      '2'
     C*
     C*
     C* PERFORM THIS CODE ONLY IF DATA ENTRY PROGRAM OPTION EXISTS IN
     C*    SDRPF (DRFL03 = 'Y')
     C*   NOTE: DERTCD = *BLANK is 'Add'/'Update')
     C*
     C     *IN34         IFEQ      '1'
     C     CMKYD         CHAIN     PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     CMFL01        IFEQ      'Y'
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C***  BEGIN CHG  ***  10/08/99  *********************************
     C****BEG*ADD *** 11/23/98 *****************************
     C**********           MOVELCMDCID    ##PGM
     C********** ##PGM     IFEQ 'F0269C1'
     C**********           MOVEL'F0269R'  ##PGM     P
     C**********           ENDIF
     C**********
     C*****BEGIN ADD  ***  05/27/99  *********************************
     C********** ##PGM     IFEQ 'W0070C'
     C**********           MOVEL'W0070R'  ##PGM     P
     C**********           ENDIF                           ##PGM=W0070C
     C*****END** ADD  ***  05/27/99  *********************************
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      *OFF          ADDINF            1
     C     ##DEIF        IFEQ      'Y'
     C     AIKY          CHAIN     PAI01                              99
     C                   ELSE
     C                   SETON                                        99
     C                   ENDIF
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *ON           ADDINF
     C                   SELECT
     C     AIDPGM        WHENEQ    'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C     AIDPGM        WHENEQ    'W0070C'
     C                   MOVEL(P)  'W0070R'      ##PGM
     C     AIDPGM        WHENEQ    'W0050C01'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   OTHER
     C                   MOVEL     AIDPGM        ##PGM
     C                   ENDSL
     C                   ELSE
     C*** END   ADD *** 06/14/04 **************************************
     C                   SELECT
     C     CMDCID        WHENEQ    'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C     CMDCID        WHENEQ    'W0070C'
     C                   MOVEL(P)  'W0070R'      ##PGM
     C     CMDCID        WHENEQ    'W0050C01'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   OTHER
     C                   MOVEL     CMDCID        ##PGM
     C                   ENDSL
     C***  END   CHG  ***  10/08/99  *********************************
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C*
     C*** END ADD *** 11/23/98 *****************************
     C***********          MOVE *BLANKS   ##PGM            DEL 11/23/98
     C                   CALL      'SRCHKPGM'
     C*** BEG CHG *** 11/23/98 *****************************
     C***********          PARM CMDCID    ##PGM  10
     C                   PARM                    ##PGM            10
     C*** END CHG *** 11/23/98 *****************************
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           *IN47
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       DEPGM             8
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ADDINF        IFEQ      *OFF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMDCID        DEPGM
     C                   MOVEL     CMFOLD        DEKEY
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   ELSE
     C                   MOVEL     AIDPGM        DEPGM
     C                   MOVEL     AIDKEY        DEKEY
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMCOMM        DECOM1
     C                   MOVE      CMCOMM        DECOM2
     C                   MOVE      ' '           DERTCD
     C*
     C                   CALL      DEPGM                                37
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
     C*
     C     *IN37         IFEQ      '1'
     C                   EXSR      ERROR
     C                   GOTO      NXT6
     C                   END
     C*
     C     DERTCD        IFEQ      'A'
     C     DERTCD        OREQ      'B'
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   EXCEPT    RCM0A
     C                   GOTO      NXT6
     C                   ELSE
     C                   MOVE      *BLANKS       CMFOLD
     C                   MOVEL     DEKEY         CMFOLD
     C                   MOVE      *BLANKS       CMCOMM
     C                   MOVEL     DECOM1        CMCOMM
     C                   MOVE      DECOM2        CMCOMM
     C                   UPDATE    PCM0A
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C* CMFL01 = 'N', DATA ENTRY PROGRAM ID DOES NOT EXIST FOR THIS
     C*    REMARK TYPE; THEREFORE, PROCESS DOC AS USUAL
     C*
     C                   ELSE
     C                   EXCEPT    RCM0A
     C                   GOTO      EDOC
     C                   END                                                    CMFL01 = 'Y'
     C*
     C                   END                                                    *IN41 = '0'
     C*
     C                   END                                                    *IN34 = '1'
     C*
     C     EDOC          TAG
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C                   MOVEA     '000000'      *IN(50)
     C                   MOVE      '0'           *IN57
     C                   MOVE      '0'           *IN59
     C                   MOVE      '0'           *IN33
     C*
     C     SLEDC         IFNE      'Y'
     C     SCIMGF        OREQ      'Y'
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SEDTDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    DRFL06
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN51
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN52
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN53
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   END
     C*
     C**** OPTION 7=DISPLAY/ADD ADDITIONAL REMARKS
     C     SCSEL         IFEQ      '7'
     C*
     C                   MOVEL     QQKEY         #7KEY            40
     C                   MOVE      SCSEQ#        #7SEQ#            5 0
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SC014R'
     C                   PARM                    #7KEY
     C                   PARM                    #7SEQ#
     C                   PARM                    QQPNAM                         12/18/91 ADD
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   END
     C*
     C*
     C**** OPTION 8=DISPLAY REMARK DETAIL
     C     SCSEL         IFEQ      '8'
     C*
     C                   MOVEL     QQKEY         #8KEY            40
     C                   MOVE      SCSEQ#        #8SEQ#            5 0
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SC012R'
     C                   PARM                    #8KEY
     C                   PARM                    #8SEQ#
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   END
     C*
     C*
     C**** OPTION 9=APPEND/DISPLAY IMAGE NOTES
     C     SCSEL         IFEQ      '9'
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   MOVEL     SCIDOC        #9KEY            40
     C                   MOVEL     'IMAGENOT'    #9PGM            10
     C                   MOVE      'ES'          #9PGM
     C                   CALL      'SCN10R'                             60
     C                   PARM                    #9KEY
     C                   PARM                    #9PGM
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN50                          SCM0513
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C                   END
     C*
     C**** OPTION 6=PRINT DOCUMENT OR IMAGE
     C     SCSEL         IFEQ      '6'
     C*
     C     SCIMGF        IFNE      'Y'                                          NOT AN IMAGE
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C     SCDCID        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END                                                    END SCDCID EQ *BLANK
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     QQKEY         XXKY             40
     C                   MOVEL     *BLANK        QQRTCD
     C*
     C     SCIMGF        IFEQ      'F'
     C                   MOVEL     SCIMGF        $MFL06            1
     C                   ELSE
     C                   MOVEL     DRFL06        $MFL06
     C                   ENDIF
     C                   CALL      'SPRTDOC'                            60
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    $MFL06
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN51
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN52
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN53
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C*
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   ELSE                                                   IS AN IMAGE
     C**** ADD 9/17/97 *** INFINITY IMAGING **********************
     C*
     C* ADD 9/28/98 *** USE IMAGE DOC ID HERE
     C                   MOVEL     SCIDOC        XXDCID
     C*** 9/28/98 ******** MOVELSCDCID    XXDCID
     C                   MOVEL     SCFOLD        XXFOLD
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     QQKEY         XXKY             40
     C                   MOVEL     *BLANK        QPRTCD            6
     C*
     C                   CALL      'SCIMGP'                             60
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM                    QPRTCD
     C*
     C*
     C***********DEL ***   MOVELSCIDOC    QPDCID 12
     C***                  MOVELQ1USER    QPUSER 10
     C***                  MOVELQ1JOB     QPTRID 10
     C***                  MOVEL*BLANK    QPRTCD  6
     C*
     C***                  CALL 'SCIMGP'               60
     C***                  PARM           QPDCID
     C***                  PARM           QPUSER
     C***                  PARM           QPTRID
     C*****************    PARM           QPRTCD
     C*
     C***** END ADD 9/17/97 *********
     C*
     C     QPRTCD        IFNE      '     0'                                       ERROR OCCURED
     C     QPRTCD        ANDNE     '000000'
     C     QPRTCD        ANDNE     '      '                                       ERROR OCCURED
     C                   MOVE      '1'           *IN59                              IMAGE
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   GOTO      NXT6
     C                   END
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C*
     C                   END                                                      IMGF <> 'Y'
     C*
     C*
     C                   END                                                      OPTION 6
     C*
     C*  Option 'P' - Print Single DARTS Remark
     C*
     C     SCSEL         IFEQ      'P'
     C*
     C                   MOVEL     QQKEY         QQKEY
     C                   MOVE      SCSEQ#        QQSEQ#            5
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQQUE            10
      *** BEGIN CHG *** 09/12/97 **************************************
     C*********************MOVEL'QPRINT'  QQQUE
     C                   MOVEL     UPOUTQ        QQQUE
      *** END   CHG *** 09/12/97 **************************************
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'SC741C'
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ#
     C                   PARM                    QQQUE
     C                   PARM                    QQMODE
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN6          XRRN6
     C                   UPDATE    SCR06S
     C*
     C                   ENDIF
     C*
     C     SCSEL         IFNE      ' '
     C     SCSEL         ANDNE     '2'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '4'
     C     SCSEL         ANDNE     '8'
     C     SCSEL         ANDNE     '9'
     C     SCSEL         ANDNE     '6'                                          12/18/91 ADD
     C     SCSEL         ANDNE     '7'                                          12/18/91 ADD
     C     SCSEL         ANDNE     'P'
     C                   MOVE      '1'           *IN54
     C                   MOVE      RRN6          XRRN6
     C*
     C                   UPDATE    SCR06S
     C                   MOVE      '1'           ERR               1
     C                   END
     C                   END
     C*
     C     NXT6          TAG
     C*
     C                   READC     SCR06S                                 60
     C                   END
     C*
     C*
     C                   MOVE      SRRN6         RRN6                           RE-APPLY SAVED RRN
     C*
     C*
     C     ERR           IFEQ      '1'
     C                   MOVE      XRRN6         #@$6
     C                   GOTO      STRT
     C                   END
     C*
     C*  Set at last record selected
     C*
     C                   Z-ADD     XRRN6         #@$6
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C*
      *------------------------*
     C     TERM          TAG
      *------------------------*
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C*** BEGIN ADD *** 06/14/04 **************************************
     C*
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*** END   ADD *** 06/14/04 **************************************
     C*** BEGIN ADD *** 10/12/98 ***********************************************
     C                   MOVEL     Q1USER        $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C                   ENDIF                                                  *IN60 EQ *ON
     C     *IN60         IFEQ      *OFF
     C     SLRACC        IFEQ      'Y'
     C                   GOTO      TERM
     C                   ENDIF                                                  SLRACC EQ 'Y'
     C                   ENDIF                                                  *IN60 EQ *OFF
     C*** END   ADD *** 10/12/98 ***********************************************
      *
      **** BEGIN ADD *** 09/12/97 *************************************
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     'QPRINT'      UPOUTQ
     C                   ENDIF
      *
      **** END   ADD *** 09/12/97 *************************************
      *
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             RRN3              5 0
     C                   Z-ADD     0             RRN6              5 0
     C                   Z-ADD     0             XRRN2             5 0
     C                   Z-ADD     0             XRRN6             5 0
     C                   Z-ADD     0             XXRRN2            5 0
     C                   Z-ADD     0             XXRRN6            5 0
     C                   Z-ADD     0             HLDRRN            5 0
     C                   Z-ADD     0             HL3RRN            5 0
     C                   Z-ADD     0             HLDRR2            5 0
     C                   Z-ADD     0             HLDRR3            5 0
     C                   Z-ADD     0             XXTIME
     C                   Z-ADD     0             XXDATE
     C                   Z-ADD     0             XXSEQ#
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********             Z-ADD*HIVAL    ##SEQ#
     C*** END DEL *** 04/25/00 ****************************************
     C                   Z-ADD     0             ##SECL
     C                   MOVE      'N'           ##SBYL
     C                   Z-ADD     0             @P
     C*
     C* INITIALIZE XXNUM.  THIS VALUE IS USED TO DESIGNATE THE NUMBER
     C*   OF LINES TO DISPLAY IN THE SUBFILE IN SUBROUTINE LSMCD
     C*   DEPENDING ON WHICH SCREEN IS BEING LOADED (SCR02, SCR03,
     C*   SCR06)
     C*
     C                   Z-ADD     0             XXNUM             5 0
     C*
     C*** BUILD KEY DISPLAY HERE
     C*
     C                   MOVEL     QQKEY         X#KEY
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL     QQPNAM        X#PNAM
     C                   CALL      'SRKCVT'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#RTCD            1
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         SCKEY
     C                   ELSE
     C                   MOVEL     QQKEY         SCKEY
     C                   END
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C*
     C*  Extract group from QQKEY
     C*
     C                   CALL      'DA001R'
     C                   PARM      QQPNAM        ##PGM            10
     C                   PARM      QQKEY         #@KEY            40
     C                   PARM      'GROUP'       ##FLD            40
     C*
     C                   MOVEL     ##FLD         XXGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   MOVE      *BLANK        UA
     C                   MOVE      *ZEROS        UD
     C                   MOVE      *ZEROS        UN
     C                   MOVE      *BLANKS       UB
     C                   MOVE      *BLANKS       DA
     C                   MOVE      *BLANKS       DD
     C                   MOVE      *BLANKS       DN
     C                   MOVE      *BLANKS       DB
     C*
     C                   MOVEL     Q1USER        $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 EQ '1'
     C                   ENDIF                                                  *IN60 EQ '1'
     C*
     C                   MOVEL     SLFOLD        SCDFLR
     C*
     C     SLCCIF        IFNE      'Y'
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END
     C*
     C*** Check System Control File Install Flags for this Client:
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   Check for AS/400 Office Installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1            Installed
     C                   MOVE      '0'           *IN03                          ADD 04/06/92
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      'N'           XXOFIF                         Installed
     C                   MOVE      '1'           *IN03                          ADD 04/06/92
     C                   END                                                    END 'Y' SYOFIF
     C*
     C                   END                                                    END 0 *IN60
     C*
     C* LOOK AT DARTS SYSTEM CONTROL FILE TO DETERMINE WHAT'S INSTALLED
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** IF IMAGE IS INSTALLED, SET ON IMAGE INDICATOR.
     C     DRFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN99
     C                   END
     C*
     C*** IF DATA ENTRY PROGRAM OPTION IS INSTALLED, SET ON INDICATOR.
     C*      (DRFL03 = 'Y', *IN34 = 1)
     C     DRFL03        IFEQ      'Y'                                          ADD 04/09/92
     C                   MOVE      '1'           *IN34
     C                   END                                                    ADD 04/09/92
     C*
     C*** IF SUMMARY CODE  OPTION IS INSTALLED, SET ON INDICATOR.
     C*      (DRFL02 = 'Y', *IN36 = 1), INITIALIZE SCSMCD TO *BLANKS
     C     DRFL02        IFEQ      'Y'                                          ADD 04/21/92
     C                   MOVE      '1'           *IN36                          ADD 04/21/92
     C                   MOVE      *BLANKS       SCSMCD                         ADD 04/21/92
     C                   END                                                    ADD 04/21/92
     C*
     C                   END                                                    END 0 *IN60
     C*
     C                   MOVEL     SLCSID        SCCSID
     C*
     C*** Office Installed, Sum Cd Installed
     C     *IN03         IFEQ      '0'
     C     *IN36         ANDEQ     '1'
     C                   Z-ADD     4             C
     C                   END
     C*** Office Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '0'
     C     *IN36         ANDEQ     '0'
     C                   Z-ADD     2             C
     C                   END
     C*** Office NOT Installed, Sum Cd Installed
     C     *IN03         IFEQ      '1'
     C     *IN36         ANDEQ     '1'
     C                   Z-ADD     3             C
     C                   END
     C*** Office NOT Installed, Sum Cd NOT Installed
     C     *IN03         IFEQ      '1'
     C     *IN36         ANDEQ     '0'
     C                   Z-ADD     1             C
     C                   END
     C*
     C                   MOVEL     CK(C)         SCCMDL
      *** BEGIN ADD *** 11/19/99 ***********************************************
      *
     C***  BEGIN CHG  ***  10/16/12  *********************************
      ******* Check for the installation of VDARTS.
      ** The check for VDARTS is no longer needed as all clients in v13+
      ** will be required to have iDARTS to integrate
     C*******     DRFL10        IFEQ      'Y'
     C                   EXSR      SRVDAR
     C*******                   ELSE
     C*******                   MOVE      ' '           XFVDTA
     C*******                   MOVE      *OFF          *IN02
     C*******                   ENDIF                                                  DRFL07 = 'Y'
     C***  END   CHG  ***  10/16/12  *********************************
      *** END   ADD *** 11/19/99 ***********************************************
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C*** GET INITIAL PATH INFORMATION
     C*
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** PPKY      CHAINPPP01                60
     C     PPKY1         CHAIN     PPP01                              60
     C   60PPKY          CHAIN     PPP01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C*
     C     *IN60         IFEQ      '0'
     C*
     C***  ALLOW OUTSIDE RANGE IF 'Y' IN PATH RCD, AND SECURITY FILE
     C*
     C                   MOVEL     PPPATH        ##PID
     C                   EXSR      PIGET
     C*
     C     PIALOR        IFEQ      'Y'
     C     SLCCIF        ANDEQ     'Y'
     C                   MOVE      '0'           *IN98
     C                   ELSE
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C                   ELSE
     C                   MOVEL     SCCSID        PA(1)
     C                   MOVEL     SCRFMT        RF(1)
     C                   END
     C*
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      H1GET
     C                   EXSR      CGGETB
     C                   ELSE                                                   ELSE EQ 'B'
     C                   EXSR      CGGETA
     C                   EXSR      HDGET
     C                   END                                                    END EQ 'B'
     C*
     C*  CHANGED INITIAL ENTRY ORDER INTO SCR02 BACK INTO 'ORDERED BY
     C*     REMARK DATE IN DESCENDING ORDER
     C*
     C***** INITIAL ENTRY INTO SCR02 IS ORDERED BY REMARK CODE STARTING
     C*******      WITH 'REMARK CODE PREFIX' IN USER'S SECURITY RECORD.
     C*
     C                   MOVE      *BLANKS       SCSRCD                         ADD 04/20/92
     C                   MOVEL     'DATE '       SORT                           ADD 07/29/92
     C                   Z-ADD     9999999       @@CDAT
     C     SCSRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCSRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@CDAT
     C*
     C                   END
     C     CMKY2         SETLL     PCM02                                        ADD 07/29/92
     C*
     C                   EXSR      LOAD2
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      SCCSID        HLCSID            2
     C                   MOVE      SCRFMT        HLRFMT
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   PIGET SUBROUTINE (PATH ID)                       H D G E T ***
      *====================================================================*
     C*
     C     PIGET         BEGSR
     C*
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** PIKY      CHAINPPI01                60
     C     PIKY1         CHAIN     PPI01                              60
     C   60PIKY          CHAIN     PPI01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CI            PA
     C                   MOVEL     FR            RF
     C*
     C                   Z-ADD     1             #P                5 0
     C                   MOVEA     '00000000'    *IN(11)
     C                   MOVEA     '00'          *IN(19)
     C*
     C                   MOVEA     '00000000'    *IN(61)
     C                   MOVEA     '00'          *IN(69)
     C                   MOVE      '1'           *IN61
     C                   MOVE      PA(#P)        SCCSID
     C                   MOVE      PA(#P)        HLCSID
     C                   MOVE      RF(#P)        SCRFMT
     C                   MOVE      RF(#P)        HLRFMT
     C                   END
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   HDGET SUBROUTINE (DATABASE FILE - FORMAT A)      H D G E T ***
      *====================================================================*
     C*
     C     HDGET         BEGSR
     C*
     C     HDKY          CHAIN     PHD01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   EXCEPT    RHD01
     C*
     C                   MOVE      HDUA01        SCUA01
     C                   MOVE      HDUA02        SCUA02
     C                   MOVE      HDUA03        SCUA03
     C                   MOVE      HDUA04        SCUA04
     C                   MOVE      HDUA05        SCUA05
     C                   MOVE      HDUA06        SCUA06
     C                   MOVE      HDUA07        SCUA07
     C                   MOVE      HDUA08        SCUA08
     C                   MOVE      HDUA09        SCUA09
     C                   MOVE      HDUA10        SCUA10
     C*
     C                   MOVE      HDUD01        SCUD01
     C                   MOVE      HDUD02        SCUD02
     C                   MOVE      HDUD03        SCUD03
     C                   MOVE      HDUD04        SCUD04
     C                   MOVE      HDUD05        SCUD05
     C*
     C                   MOVE      HDUN01        SCUN01
     C                   MOVE      HDUN02        SCUN02
     C                   MOVE      HDUN03        SCUN03
     C                   MOVE      HDUN04        SCUN04
     C                   MOVE      HDUN05        SCUN05
     C*
     C                   ELSE
     C*
     C*
     C                   MOVE      *BLANK        SCUA01
     C                   MOVE      *BLANK        SCUA02
     C                   MOVE      *BLANK        SCUA03
     C                   MOVE      *BLANK        SCUA04
     C                   MOVE      *BLANK        SCUA05
     C                   MOVE      *BLANK        SCUA06
     C                   MOVE      *BLANK        SCUA07
     C                   MOVE      *BLANK        SCUA08
     C                   MOVE      *BLANK        SCUA09
     C                   MOVE      *BLANK        SCUA10
     C*
     C                   MOVE      *ZERO         SCUD01
     C                   MOVE      *ZERO         SCUD02
     C                   MOVE      *ZERO         SCUD03
     C                   MOVE      *ZERO         SCUD04
     C                   MOVE      *ZERO         SCUD05
     C*
     C                   MOVE      *ZERO         SCUN01
     C                   MOVE      *ZERO         SCUN02
     C                   MOVE      *ZERO         SCUN03
     C                   MOVE      *ZERO         SCUN04
     C                   MOVE      *ZERO         SCUN05
     C*
     C                   END
     C                   ENDSR
     C*
      *====================================================================*
      * * *   H1GET SUBROUTINE (DATABASE FILE - FORMAT B)      H 1 G E T ***
      *====================================================================*
     C     H1GET         BEGSR
     C*
     C     HDKY          CHAIN     PH101                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RH101
     C*
     C                   MOVE      H1UB01        SCUB01
     C                   MOVE      H1UB02        SCUB02
     C                   MOVE      H1UB03        SCUB03
     C                   MOVE      H1UB04        SCUB04
     C                   MOVE      H1UB05        SCUB05
     C                   MOVE      H1UB06        SCUB06
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        SCUB01
     C                   MOVE      *BLANK        SCUB02
     C                   MOVE      *BLANK        SCUB03
     C                   MOVE      *BLANK        SCUB04
     C                   MOVE      *BLANK        SCUB05
     C                   MOVE      *BLANK        SCUB06
     C*
     C                   END
     C*
     C                   ENDSR
      *====================================================================*
      * * *   CGGETA SUBROUTINE (CUSTOM GROUP ID - FORMAT A)    C G G E T A ***
      *====================================================================*
     C*
     C     CGGETA        BEGSR
     C*
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** CGKY      CHAINPCG01                60
     C                   CLEAR                   CGGRP
     C     CGKY1         CHAIN     PCG01                              60
     C   60CGKY          CHAIN     PCG01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      CGDA01        SCDA01
     C                   MOVE      CGDA02        SCDA02
     C                   MOVE      CGDA03        SCDA03
     C                   MOVE      CGDA04        SCDA04
     C                   MOVE      CGDA05        SCDA05
     C                   MOVE      CGDA06        SCDA06
     C                   MOVE      CGDA07        SCDA07
     C                   MOVE      CGDA08        SCDA08
     C                   MOVE      CGDA09        SCDA09
     C                   MOVE      CGDA10        SCDA10
     C*
     C                   MOVE      CGDD01        SCDD01
     C                   MOVE      CGDD02        SCDD02
     C                   MOVE      CGDD03        SCDD03
     C                   MOVE      CGDD04        SCDD04
     C                   MOVE      CGDD05        SCDD05
     C*
     C                   MOVE      CGDN01        SCDN01
     C                   MOVE      CGDN02        SCDN02
     C                   MOVE      CGDN03        SCDN03
     C                   MOVE      CGDN04        SCDN04
     C                   MOVE      CGDN05        SCDN05
     C*
     C                   MOVEL     CGDESC        SCCGD2
     C*
     C     CGRCDS        IFGT      SLSECL
     C                   MOVE      '1'           ERRSEC            1
     C                   ELSE
     C                   MOVE      '0'           ERRSEC
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        SCDA01
     C                   MOVE      *BLANK        SCDA02
     C                   MOVE      *BLANK        SCDA03
     C                   MOVE      *BLANK        SCDA04
     C                   MOVE      *BLANK        SCDA05
     C                   MOVE      *BLANK        SCDA06
     C                   MOVE      *BLANK        SCDA07
     C                   MOVE      *BLANK        SCDA08
     C                   MOVE      *BLANK        SCDA09
     C                   MOVE      *BLANK        SCDA10
     C*
     C                   MOVE      *BLANK        SCDD01
     C                   MOVE      *BLANK        SCDD02
     C                   MOVE      *BLANK        SCDD03
     C                   MOVE      *BLANK        SCDD04
     C                   MOVE      *BLANK        SCDD05
     C*
     C                   MOVE      *BLANK        SCDN01
     C                   MOVE      *BLANK        SCDN02
     C                   MOVE      *BLANK        SCDN03
     C                   MOVE      *BLANK        SCDN04
     C                   MOVE      *BLANK        SCDN05
     C*
     C                   END
     C*
     C*
     C* * * * WHICH ALPHA FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     70            Y
     C*
     C                   DO        10            X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DA(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SA(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DA(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C*
     C* * * * WHICH NUMERIC FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     80            Y
     C*
     C                   DO        5             X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DD(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SD(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DD(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C*
     C* * * * WHICH AMOUNT FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     85            Y
     C*
     C                   DO        5             X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DN(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SN(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DN(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '0'           *IN55
     C                   ENDSR
     C*
      *====================================================================*
      * * *   CGGETB SUBROUTINE (CUSTOM GROUP ID - FORMAT B)    C G G E T B ***
      *====================================================================*
     C     CGGETB        BEGSR
     C*
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** CGKY      CHAINPCG01                60
     C                   CLEAR                   CGGRP
     C     CGKY1         CHAIN     PCG01                              60
     C   60CGKY          CHAIN     PCG01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      CGDB01        SCDB01
     C                   MOVE      CGDB02        SCDB02
     C                   MOVE      CGDB03        SCDB03
     C                   MOVE      CGDB04        SCDB04
     C                   MOVE      CGDB05        SCDB05
     C                   MOVE      CGDB06        SCDB06
     C*
     C                   MOVEL     CGDESC        SCCGD2
     C*
     C     CGRCDS        IFGT      SLSECL
     C                   MOVE      '1'           ERRSEC            1
     C                   ELSE
     C                   MOVE      '0'           ERRSEC
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        SCDB01
     C                   MOVE      *BLANK        SCDB02
     C                   MOVE      *BLANK        SCDB03
     C                   MOVE      *BLANK        SCDB04
     C                   MOVE      *BLANK        SCDB05
     C                   MOVE      *BLANK        SCDB06
     C*
     C                   END
     C*
     C* * * * WHICH (35A) ALPHA FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     70            Y
     C*
     C                   DO        6             X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DB(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SB(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DB(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '1'           *IN55
     C                   ENDSR
      *====================================================================*
      * * *   LOAD2 SUBROUTINE (SCR02)                         L O A D 2 ***
      *====================================================================*
     C*
     C     LOAD2         BEGSR
     C*
     C                   MOVE      '0'           *IN02
     C                   MOVE      '0'           *IN05                          SFLEND * NO *
     C                   MOVE      '0'           *IN32                               END OF FILE
     C*
     C*  Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
      *  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 4 LINES
     C*
     C                   DO        4             X                 5 0
     C*
     C     NXTCM1        TAG
     C     SORT          IFEQ      'DATE '
     C     CMKY          READE     PCM02                                  60
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY          READE     PCM03                                  60
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY          READE     PCM04                                  60
     C                   ELSE
     C     CMKY          READE     PCM01                                  60
     C                   END
     C                   END
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32                               END OF FILE
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CMSBYL        IFEQ      'Y'
     C     CMSECL        ANDGT     SLSECL
     C                   GOTO      NXTCM1
     C                   END
     C*
     C***** BEGIN ADD ********* 05/07/07 *****************************
     C     @DCID2        IFEQ      'WC'
     C     CMRMCD        ANDEQ     'PSPP'
     C     CMPGM         ANDEQ     'W7413R'
     C                   GOTO      NXTCM1
     C                   ENDIF
     C***** END   ADD ********* 05/07/07 *****************************
     C*
     C                   ADD       1             RRN2
     C                   ADD       1             RRN3
     C                   ADD       1             RRN6
     C*
     C*  Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     CMCOMM        SCCO45
     C                   MOVEL     CMCOMM        SCCOMM
     C*
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RAR01
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCO45
     C                   MOVE      XXMORE        SCCOMM
     C                   END
     C*
     C                   MOVEL     CMDCID        SCDCID
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     SCDCID        ANDEQ     *BLANKS
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIDPGM        SCDCID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMRMCD        SCCODE
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     CMSEQ#        SCSEQ#
     C                   MOVEL     CMFOLD        SCFOLD
     C                   MOVEL     CMSEQ#        SCSQ#
     C*
     C     CMIMGF        IFEQ      'Y'
     C                   MOVE      '1'           *IN02                          IMAGE TYPE INDICATOR
     C                   MOVEL     CMIMGF        SCIMGF                         IMAGE FLAG
     C                   MOVEL     'Img:'        SCDOCH
     C                   ELSE
     C                   MOVE      '0'           *IN02
     C     CMIMGF        IFNE      *BLANKS
     C                   MOVEL     CMIMGF        SCIMGF                         IMAGE FLAG
     C                   ELSE
     C                   MOVE      'N'           SCIMGF
     C                   ENDIF
     C                   MOVEL     'Doc:'        SCDOCH
     C                   END
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
     C*
     C                   MOVEL     CMFL05        SCFL05                             HIDDEN
     C*
     C                   MOVE      '0'           *IN06
     C*
     C*  If remark is a completed Diary remark, highlight remark date.
     C*
     C*** Begin change 05/22/01 ***************************************
     C******               MOVE *OFF      *IN28
     C                   MOVE      *OFF          *IN97
     C     CMDIFL        IFEQ      'Y'
     C     CMDCDT        ANDGT     *ZERO
     C******               MOVE *ON       *IN28
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*** End   change 05/22/01 ***************************************
     C*
     C                   WRITE     SCR02S
     C                   WRITE     SCR03S
     C                   WRITE     SCR06S
     C*
     C                   ELSE                                                   60 = '0'
     C*
     C                   MOVE      '1'           *IN05                          SFLEND *
     C*
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     *BLANK        SCCO45
     C                   MOVEL     *BLANK        SCCOMM
     C                   MOVEL     *BLANK        SCDCID
     C                   MOVEL     *BLANK        SCCODE
     C                   Z-ADD     0             SCCDAT
     C                   MOVEL     000000        SCSEQ#
     C                   MOVEL     *ZERO         SCSQ#
     C                   MOVEL     *BLANK        SCFOLD
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        SCIQUE                             HIDDEN
     C                   MOVEL     *BLANK        SCIPTH                             HIDDEN
     C                   MOVE      *ZEROS        SCPIDX                             HIDDEN
     C                   MOVEL     *BLANK        SCAPID                             HIDDEN
     C                   MOVEL     *BLANK        SCIDOC                             HIDDEN
     C                   MOVEL     *BLANK        SCIMGF                             HIDDEN
     C                   MOVEL     *BLANK        SCQPTY                             HIDDEN
     C                   MOVEL     *BLANK        SCDCTP                             HIDDEN
     C*
     C*
     C                   MOVEL     *BLANK        SCFL05                             HIDDEN
     C*
     C                   MOVE      '1'           *IN06
     C*
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             RRN3
     C                   Z-ADD     1             RRN6
     C                   Z-ADD     1             XXRRN2
     C                   Z-ADD     1             XXRRN6
     C                   MOVE      *OFF          *IN28
     C*
     C                   WRITE     SCR02S
     C                   WRITE     SCR03S
     C                   WRITE     SCR06S
     C*
     C                   END
     C*
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     XXRRN2        #@$2
     C                   Z-ADD     XXRRN2        #@$3
     C                   Z-ADD     XXRRN2        #@$6
     C                   Z-ADD     XXRRN2        XRRN2
     C                   Z-ADD     XXRRN2        XRRN6
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   LOAD3  SUBROUTINE (SCR03)                        L O A D 3 ***
      *====================================================================*
     C*
     C     LOAD3         BEGSR
     C*
     C*
     C                   MOVE      '0'           *IN02
     C                   MOVE      '0'           *IN05                          SFLEND * NO *
     C                   MOVE      '0'           *IN32
     C*
      *  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 8 LINES
     C*
     C                   DO        2             X                 5 0
     C*
     C     NXTCM2        TAG
     C     SORT          IFEQ      'DATE '
     C     CMKY          READE     PCM02                                  60
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY          READE     PCM03                                  60
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY          READE     PCM04                                  60
     C                   ELSE
     C     CMKY          READE     PCM01                                  60
     C                   END
     C                   END
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32                               END OF FILE
     C                   END
     C*
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CMSBYL        IFEQ      'Y'
     C     CMSECL        ANDGT     SLSECL
     C                   GOTO      NXTCM2
     C                   END
     C*
     C*
     C                   ADD       1             RRN3
     C                   ADD       1             RRN2
     C                   ADD       1             RRN6
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     CMCOMM        SCCO45
     C                   MOVEL     CMCOMM        SCCOMM
     C*
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RAR01
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCO45
     C                   MOVE      XXMORE        SCCOMM
     C                   END
     C*
     C                   MOVEL     CMDCID        SCDCID
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     SCDCID        ANDEQ     *BLANKS
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIDPGM        SCDCID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMRMCD        SCCODE
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     CMSEQ#        SCSEQ#
     C                   MOVEL     CMFOLD        SCFOLD
     C                   MOVEL     CMSEQ#        SCSQ#
     C*
     C     CMIMGF        IFEQ      'Y'
     C                   MOVE      '1'           *IN02                          IMAGE TYPE INDICATOR
     C                   MOVE      'Y'           SCIMGF                         IMAGE FLAG
     C                   MOVEL     'Img:'        SCDOCH
     C                   ELSE
     C                   MOVE      '0'           *IN02
     C     CMIMGF        IFNE      *BLANKS
     C                   MOVEL     CMIMGF        SCIMGF                         IMAGE FLAG
     C                   ELSE
     C                   MOVE      'N'           SCIMGF
     C                   ENDIF
     C                   MOVEL     'Doc:'        SCDOCH
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C*
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
     C                   MOVEL     CMFL05        SCFL05                             HIDDEN
     C*
     C*  If remark is a completed Diary remark, highlight remark date.
     C*
     C*** Begin change 05/22/01 ***************************************
     C******               MOVE *OFF      *IN28
     C                   MOVE      *OFF          *IN97
     C     CMDIFL        IFEQ      'Y'
     C     CMDCDT        ANDGT     *ZERO
     C******               MOVE *ON       *IN28
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*** End   change 05/22/01 ***************************************
     C*
     C                   WRITE     SCR03S
     C                   WRITE     SCR02S
     C                   WRITE     SCR06S
     C*
     C*
     C                   ELSE                                                   60 = '0'
     C*
     C                   MOVE      '1'           *IN05                          SFLEND *
     C*
     C                   END
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C*
     C                   Z-ADD     RRN3          #@$2
     C                   Z-ADD     RRN3          #@$3
     C                   Z-ADD     RRN3          #@$6
     C*
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   LOAD6 SUBROUTINE   (SCR06)                       L O A D 6 ***
      *====================================================================*
     C*
     C     LOAD6         BEGSR
     C*
     C*
     C                   MOVE      '0'           *IN02
     C                   MOVE      '0'           *IN05                          SFLEND * NO *
     C                   MOVE      '0'           *IN32
     C*
     C*  Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
      *  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 14 LINES
     C*
     C                   DO        14            X                 5 0
     C*
     C     NXTCM3        TAG
     C     SORT          IFEQ      'DATE '
     C     CMKY          READE     PCM02                                  60
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY          READE     PCM03                                  60
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY          READE     PCM04                                  60
     C                   ELSE
     C     CMKY          READE     PCM01                                  60
     C                   END
     C                   END
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32                               END OF FILE
     C                   END
     C*
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CMSBYL        IFEQ      'Y'
     C     CMSECL        ANDGT     SLSECL
     C                   GOTO      NXTCM3
     C                   END
      *
     C***** BEGIN ADD ********* 05/07/07 *****************************
     C     @DCID2        IFEQ      'WC'
     C     CMRMCD        ANDEQ     'PSPP'
     C     CMPGM         ANDEQ     'W7413R'
     C                   GOTO      NXTCM3
     C                   ENDIF
     C***** END   ADD ********* 05/07/07 *****************************
     C*
     C                   ADD       1             RRN2
     C                   ADD       1             RRN3
     C                   ADD       1             RRN6
     C*
     C*  Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN6          XXRRN6
     C                   MOVE      'N'           PCFL
     C                   END
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     CMCOMM        SCCO45
     C                   MOVEL     CMCOMM        SCCOMM
     C*
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RAR01
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCO45
     C                   MOVE      XXMORE        SCCOMM
     C                   END
     C*
     C                   MOVEL     CMDCID        SCDCID
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     SCDCID        ANDEQ     *BLANKS
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIDPGM        SCDCID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMRMCD        SCCODE
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     CMSEQ#        SCSEQ#
     C                   MOVEL     CMFOLD        SCFOLD
     C                   MOVEL     CMSEQ#        SCSQ#
     C*
     C     CMIMGF        IFEQ      'Y'
     C                   MOVE      '1'           *IN02                          IMAGE TYPE INDICATOR
     C                   MOVE      'Y'           SCIMGF                         IMAGE FLAG
     C                   MOVEL     'Img:'        SCDOCH
     C                   ELSE
     C                   MOVE      '0'           *IN02
     C     CMIMGF        IFNE      *BLANKS
     C                   MOVEL     CMIMGF        SCIMGF                         IMAGE FLAG
     C                   ELSE
     C                   MOVE      'N'           SCIMGF
     C                   ENDIF
     C                   MOVEL     'Doc:'        SCDOCH
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C*
     C*
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
     C*
     C                   MOVEL     CMFL05        SCFL05                             HIDDEN
     C*
     C*  If remark is a completed Diary remark, highlight remark date.
     C*
     C*** Begin change 05/22/01 ***************************************
     C******               MOVE *OFF      *IN28
     C                   MOVE      *OFF          *IN97
     C     CMDIFL        IFEQ      'Y'
     C     CMDCDT        ANDGT     *ZERO
     C******               MOVE *ON       *IN28
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*** End   change 05/22/01 ***************************************
     C*
     C                   WRITE     SCR02S
     C                   WRITE     SCR03S
     C                   WRITE     SCR06S
     C*
     C*
     C                   ELSE                                                   60 = '0'
     C*
     C                   MOVE      '1'           *IN05                          SFLEND *
     C*
     C*
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN6          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     *BLANK        SCCO45
     C                   MOVEL     *BLANK        SCCOMM
     C                   MOVEL     *BLANK        SCDCID
     C                   MOVEL     *BLANK        SCCODE
     C                   Z-ADD     0             SCCDAT
     C                   MOVEL     000000        SCSEQ#
     C                   MOVEL     *ZERO         SCSQ#
     C                   MOVEL     *BLANK        SCFOLD
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        SCIQUE                             HIDDEN
     C                   MOVEL     *BLANK        SCIPTH                             HIDDEN
     C                   MOVE      *ZEROS        SCPIDX                             HIDDEN
     C                   MOVEL     *BLANK        SCAPID                             HIDDEN
     C                   MOVEL     *BLANK        SCIDOC                             HIDDEN
     C                   MOVEL     *BLANK        SCIMGF                             HIDDEN
     C                   MOVEL     *BLANK        SCQPTY                             HIDDEN
     C                   MOVEL     *BLANK        SCDCTP                             HIDDEN
     C*
     C                   MOVEL     *BLANK        SCFL05                             HIDDEN
     C*
     C                   MOVE      '1'           *IN06
     C*
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             RRN3
     C                   Z-ADD     1             RRN6
     C*
     C                   Z-ADD     1             XXRRN2
     C                   Z-ADD     1             XXRRN6
     C*
     C*  If remark is a completed Diary remark, highlight remark date.
     C*
     C*** Begin change 05/22/01 ***************************************
     C******               MOVE *OFF      *IN28
     C                   MOVE      *OFF          *IN97
     C     CMDIFL        IFEQ      'Y'
     C     CMDCDT        ANDGT     *ZERO
     C******               MOVE *ON       *IN28
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*** Begin change 05/22/01 ***************************************
     C*
     C                   WRITE     SCR02S
     C                   WRITE     SCR03S
     C                   WRITE     SCR06S
     C*
     C                   END
     C*
     C*
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     XXRRN6        #@$2
     C                   Z-ADD     XXRRN6        #@$3
     C                   Z-ADD     XXRRN6        #@$6
     C                   Z-ADD     XXRRN6        XRRN2
     C                   Z-ADD     XXRRN6        XRRN6
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   LSMCD SUBROUTINE (LOAD SCR02, SCR03, SCR06 IN SMCD ORDER)6 ***
      *====================================================================*
     C*
     C     LSMCD         BEGSR
     C*
     C                   MOVE      '0'           *IN02
     C                   MOVE      '0'           *IN05                          SFLEND * NO *
     C                   MOVE      '0'           *IN32
     C*
     C*  Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
      *  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 14 LINES
     C*
     C                   DO        XXNUM         X                 5 0
     C*
     C     NXTCM         TAG
     C     SORT          IFEQ      'DATE '
     C     CMKY          READE     PCM02                                  60
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY          READE     PCM03                                  60
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY          READE     PCM04                                  60
     C                   ELSE
     C     CMKY          READE     PCM01                                  60
     C                   END
     C                   END
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32                               END OF FILE
     C                   END
     C*
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CMSBYL        IFEQ      'Y'
     C     CMSECL        ANDGT     SLSECL
     C                   GOTO      NXTCM
     C                   END
     C*
     C     CTKY          CHAIN     PCT01                              95
     C     *IN95         IFEQ      '0'
     C     CTSMCD        ANDEQ     SCSMCD
     C*
     C                   ADD       1             RRN2
     C                   ADD       1             RRN3
     C                   ADD       1             RRN6
     C*
     C*  Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   Z-ADD     RRN6          XXRRN6
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     CMCOMM        SCCO45
     C                   MOVEL     CMCOMM        SCCOMM
     C*
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RAR01
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCO45
     C                   MOVE      XXMORE        SCCOMM
     C                   END
     C*
     C                   MOVEL     CMDCID        SCDCID
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     SCDCID        ANDEQ     *BLANKS
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIDPGM        SCDCID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMRMCD        SCCODE
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     CMSEQ#        SCSEQ#
     C                   MOVEL     CMFOLD        SCFOLD
     C                   MOVEL     CMSEQ#        SCSQ#
     C*
     C*
     C     CMIMGF        IFEQ      'Y'
     C                   MOVE      '1'           *IN02                          IMAGE TYPE INDICATOR
     C                   MOVE      'Y'           SCIMGF                         IMAGE FLAG
     C                   MOVEL     'Img:'        SCDOCH
     C                   ELSE
     C                   MOVE      '0'           *IN02
     C     CMIMGF        IFNE      *BLANKS
     C                   MOVEL     CMIMGF        SCIMGF                         IMAGE FLAG
     C                   ELSE
     C                   MOVE      'N'           SCIMGF
     C                   ENDIF
     C                   MOVEL     'Doc:'        SCDOCH
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C*
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
     C                   MOVEL     CMFL05        SCFL05                             HIDDEN
     C*
     C*
     C*  If remark is a completed Diary remark, highlight remark date.
     C*
     C*** Begin change 05/22/01 ***************************************
     C******               MOVE *OFF      *IN28
     C                   MOVE      *OFF          *IN97
     C     CMDIFL        IFEQ      'Y'
     C     CMDCDT        ANDGT     *ZERO
     C******               MOVE *ON       *IN28
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*** End   change 05/22/01 ***************************************
     C*
     C                   WRITE     SCR02S
     C                   WRITE     SCR03S
     C                   WRITE     SCR06S
     C*
     C                   ELSE
     C                   GOTO      NXTCM
     C                   END
     C*
     C                   ELSE                                                   60 = '0'
     C*
     C                   MOVE      '1'           *IN05                          SFLEND *
     C*
     C*
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C     RRN3          OREQ      0
     C     *IN60         ANDEQ     '1'
     C     RRN6          OREQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     *BLANK        SCCO45
     C                   MOVEL     *BLANK        SCCOMM
     C                   MOVEL     *BLANK        SCDCID
     C                   MOVEL     *BLANK        SCCODE
     C                   Z-ADD     0             SCCDAT
     C                   MOVEL     000000        SCSEQ#
     C                   MOVEL     *ZERO         SCSQ#
     C                   MOVEL     *BLANK        SCFOLD
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        SCIQUE                             HIDDEN
     C                   MOVEL     *BLANK        SCIPTH                             HIDDEN
     C                   MOVE      *ZEROS        SCPIDX                             HIDDEN
     C                   MOVEL     *BLANK        SCAPID                             HIDDEN
     C                   MOVEL     *BLANK        SCIDOC                             HIDDEN
     C                   MOVEL     *BLANK        SCIMGF                             HIDDEN
     C                   MOVEL     *BLANK        SCQPTY                             HIDDEN
     C                   MOVEL     *BLANK        SCDCTP                             HIDDEN
     C                   MOVEL     *BLANK        SCFL05                             HIDDEN
     C*
     C*
     C*
     C                   MOVE      '1'           *IN06
     C*
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             RRN3
     C                   Z-ADD     1             RRN6
     C                   Z-ADD     1             XXRRN2
     C                   Z-ADD     1             XXRRN6
     C                   MOVE      *OFF          *IN28
     C*
     C                   WRITE     SCR02S
     C                   WRITE     SCR03S
     C                   WRITE     SCR06S
     C*
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C   92              Z-ADD     XXRRN2        #@$2
     C   92              Z-ADD     XXRRN2        #@$3
     C   92              Z-ADD     XXRRN2        #@$6
     C   92              Z-ADD     XXRRN2        XRRN2
     C   92              Z-ADD     XXRRN2        XRRN6
     C*
     C   93              Z-ADD     RRN3          #@$2
     C   93              Z-ADD     RRN3          #@$3
     C   93              Z-ADD     RRN3          #@$6
     C*
     C   94              Z-ADD     XXRRN6        #@$2
     C   94              Z-ADD     XXRRN6        #@$3
     C   94              Z-ADD     XXRRN6        #@$6
     C   94              Z-ADD     XXRRN6        XRRN2
     C   94              Z-ADD     XXRRN6        XRRN6
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   RELOD  SUBROUTINE                                R E L O D ***
      *====================================================================*
     C*
     C     RELOD         BEGSR
     C*
     C                   MOVE      '0'           *IN02
     C                   MOVE      '0'           *IN05                          SFLEND * NO *
     C*
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             RRN3              5 0
     C                   Z-ADD     0             RRN6              5 0
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   WRITE     SCR03C
     C                   WRITE     SCR06C
     C                   MOVE      '0'           *IN04
     C*
     C*
     C     SORT          IFEQ      'DATE '
     C                   Z-ADD     9999999       @@CDAT
     C     SCSRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCSRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@CDAT
     C*
     C                   END
     C     CMKY2         SETLL     PCM02                                  60
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY3         SETLL     PCM03                                  60
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY4         SETLL     PCM04                                  60
     C                   ELSE
     C     CMKY1         SETLL     PCM01                                  60
     C                   END
     C                   END
     C                   END
     C*
     C* * * SETUP TO DISPLAY LAST PAGE OF PREVIOUS SUBFILE.
     C                   Z-ADD     HLRN          #@$2
     C                   Z-ADD     HLRN          #@$3
     C                   Z-ADD     HLRN          #@$6
     C*
      *  *  *  *  LOAD THE SUBFILE SCREEN
     C*
     C                   DO        HLRN          X                 5 0
     C*
     C     NXTCM4        TAG
     C     SORT          IFEQ      'DATE '
     C     CMKY          READE     PCM02                                  60
     C                   ELSE
     C     SORT          IFEQ      'TYPE '
     C     CMKY          READE     PCM03                                  60
     C                   ELSE
     C     SORT          IFEQ      'DOCID'
     C     CMKY          READE     PCM04                                  60
     C                   ELSE
     C     CMKY          READE     PCM01                                  60
     C                   END
     C                   END
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CMSBYL        IFEQ      'Y'
     C     CMSECL        ANDGT     SLSECL
     C                   GOTO      NXTCM4
     C                   END
     C*
     C                   ADD       1             RRN3
     C                   ADD       1             RRN2
     C                   ADD       1             RRN6
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     CMCOMM        SCCO45
     C                   MOVEL     CMCOMM        SCCOMM
     C*
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RAR01
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCO45
     C                   MOVE      XXMORE        SCCOMM
     C                   END
     C*
     C                   MOVEL     CMDCID        SCDCID
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     SCDCID        ANDEQ     *BLANKS
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIDPGM        SCDCID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMRMCD        SCCODE
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   MOVEL     CMSEQ#        SCSEQ#
     C                   MOVEL     CMFOLD        SCFOLD
     C                   MOVEL     CMSEQ#        SCSQ#
     C*
     C                   MOVE      '0'           *IN06
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
     C                   MOVEL     CMIMGF        SCIMGF                             HIDDEN
     C                   MOVEL     CMFL05        SCFL05                             HIDDEN
     C*
     C*
     C*  If remark is a completed Diary remark, highlight remark date.
     C*
     C*** Begin change 05/22/01 ***************************************
     C******               MOVE *OFF      *IN28
     C                   MOVE      *OFF          *IN97
     C     CMDIFL        IFEQ      'Y'
     C     CMDCDT        ANDGT     *ZERO
     C******               MOVE *ON       *IN28
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*** End   change 05/22/01 ***************************************
     C*
     C                   WRITE     SCR03S
     C                   WRITE     SCR06S
     C                   WRITE     SCR02S
     C*
     C*
     C                   ELSE                                                   60 = '0'
     C*
     C                   MOVE      '1'           *IN05                          SFLEND *
     C*
     C                   END
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C*
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   WRITEA SUBROUTINE                                 W R I T E A***
      *====================================================================*
     C*
     C     WRITEA        BEGSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C*
     C                   Z-ADD     DSTIME        HHTIME            6 0
     C*
     C     HDKY          CHAIN     PHD01                              60
     C*
     C                   MOVEL     QQKEY         HDKEY
     C*
     C                   MOVEL     SCCSID        HDCSID
     C*
     C                   MOVEL     QQPNAM        HDPNAM
     C*
     C                   MOVEL     SCUA01        HDUA01
     C                   MOVEL     SCUA02        HDUA02
     C                   MOVEL     SCUA03        HDUA03
     C                   MOVEL     SCUA04        HDUA04
     C                   MOVEL     SCUA05        HDUA05
     C                   MOVEL     SCUA06        HDUA06
     C                   MOVEL     SCUA07        HDUA07
     C                   MOVEL     SCUA08        HDUA08
     C                   MOVEL     SCUA09        HDUA09
     C                   MOVEL     SCUA10        HDUA10
     C*
     C                   MOVE      SCUD01        HDUD01
     C                   MOVE      SCUD02        HDUD02
     C                   MOVE      SCUD03        HDUD03
     C                   MOVE      SCUD04        HDUD04
     C                   MOVE      SCUD05        HDUD05
     C*
     C                   MOVE      SCUN01        HDUN01
     C                   MOVE      SCUN02        HDUN02
     C                   MOVE      SCUN03        HDUN03
     C                   MOVE      SCUN04        HDUN04
     C                   MOVE      SCUN05        HDUN05
     C*
     C                   MOVEL     *BLANK        HDSTAT
     C*
     C                   MOVEL     Q1USER        HDUSER
     C                   MOVEL     Q1PGM         HDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HDDT
     C*
     C                   Z-ADD     DSTIME        HDTIME
     C                   MOVEL     SCRFMT        HDRFMT
     C*
      *** BEG ADD ***** 03/03/05 ***********************************************
     C                   MOVE(P)   CGGRP         HDGRP
      *** END ADD ***** 03/03/05 ***********************************************
     C*
     C* * * WRITE HEADER INFO TO FILE * * *
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PHD01
     C                   ELSE
     C                   MOVEL     Q1USER        HDIUSR
     C                   WRITE     PHD01
     C                   END
     C*
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   WRITEB SUBROUTINE                                 W R I T E B***
      *====================================================================*
     C     WRITEB        BEGSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C*
     C                   Z-ADD     DSTIME        HHTIME            6 0
     C*
     C     HDKY          CHAIN     PH101                              60
     C*
     C                   MOVEL     QQKEY         H1KEY
     C*
     C                   MOVEL     SCCSID        H1CSID
     C*
     C                   MOVEL     QQPNAM        H1PNAM
     C*
     C                   MOVEL     SCUB01        H1UB01
     C                   MOVEL     SCUB02        H1UB02
     C                   MOVEL     SCUB03        H1UB03
     C                   MOVEL     SCUB04        H1UB04
     C                   MOVEL     SCUB05        H1UB05
     C                   MOVEL     SCUB06        H1UB06
     C*
     C                   MOVEL     *BLANK        H1STAT
     C*
     C                   MOVEL     Q1USER        H1USER
     C                   MOVEL     Q1PGM         H1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1DT
     C*
     C                   Z-ADD     DSTIME        H1TIME
     C                   MOVEL     SCRFMT        H1RFMT
     C*
      *** BEG ADD ***** 03/03/05 ***********************************************
     C                   MOVE(P)   CGGRP         H1GRP
      *** END ADD ***** 03/03/05 ***********************************************
     C*
     C* * * WRITE HEADER INFO TO FILE * * *
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PH101
     C                   ELSE
     C                   MOVEL     Q1USER        H1IUSR
     C                   WRITE     PH101
     C                   END
     C*
     C*
     C                   ENDSR
      *====================================================================*
      * * *   WCOM  SUBROUTINE - WRITE REMARK RECORD           W C O M   ***
      *====================================================================*
     C*
     C     WCOM          BEGSR
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     HHDT
     C********
     C********             Z-ADDDSTIME    HHTIME  60
     C******WRITE REMARKS  TO FILE * * *
     C********
     C********             MOVE ##SECL    CMSECL
     C********             MOVE ##SBYL    CMSBYL
     C********
     C********             MOVELQQKEY     CMKEY
     C********             MOVELSCCSID    CMCSID
     C********             MOVELQQPNAM    CMPNAM
     C********             MOVE *BLANKS   CMCODE
     C********             MOVELSCCODE    CMRMCD
     C********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********
     C********   CMRMCD    CHAINPCT01                60
     C********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C****BEGIN ADD *** 05/18/99 ***********************************************
     C********             CLEARCMDCID
     C****END*  ADD *** 05/18/99 ***********************************************
     C********   XXDEWR    IFEQ 'Y'
     C********             MOVELCTDEPG    CMDCID
     C****BEGIN ADD *** 02/01/98 ***********************************************
     C********   SCCOMM    IFEQ *BLANKS
     C****END*ADD ***** 02/01/98 ***********************************************
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVELDECOM1    CMCOMM
     C********             MOVE DECOM2    CMCOMM
     C****BEGIN ADD *** 02/01/98 ***********************************************
     C********             ELSE
     C********             MOVELSCCOMM    CMCOMM    P
     C********             END
     C********             MOVELQQKEY     CMFOLD
     C****END*ADD ***** 02/01/98 ***********************************************
     C****BEGIN DLT *** 02/01/98 ***********************************************
     C******************** MOVELDEKEY     CMFOLD
     C****END*DLT ***** 02/01/98 ***********************************************
     C********             MOVEL'Y'       CMFL01
     C********             MOVEL'N'       XXDEWR
     C********             ELSE
     C********             MOVELSCDCID    CMDCID
     C********             MOVELSCCOMM    CMCOMM
     C********             MOVE ' '       CMFL01
     C********             MOVELSCDFLR    CMFOLD
     C********             END
     C********
     C********             Z-ADDSCCDAT    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********
     C********             Z-ADDHHDT      CMTRDT
     C********             Z-ADDHHTIME    CMTRTM
     C********
     C******IMAGE FIELDS
     C********             MOVELSCIQUE    CMIQUE               HIDDEN
     C********             MOVELSCIPTH    CMIPTH               HIDDEN
     C********             MOVE SCPIDX    CMPIDX               HIDDEN
     C********             MOVELSCAPID    CMAPID               HIDDEN
     C********             MOVELSCIDOC    CMIDOC               HIDDEN
     C********             MOVELSCQPTY    CMQPTY               HIDDEN
     C********             MOVELSCDCTP    CMDCTP               HIDDEN
     C********             MOVELSCIMGF    CMIMGF               HIDDEN
     C********
     C********
     C********             MOVEL*BLANK    CMSTAT
     C********             MOVELQ1PGM     CMPGM
     C********
     C****IF*DIARY USER ENTERED, WRITE TO CMUSER
     C********   SCDUSR    IFNE *BLANKS
     C********             MOVELSCDUSR    CMUSER
     C********             ELSE                            ELSE NE *BLANKS
     C****ELSE DEFAULT TO CURRENT USER
     C********             MOVELQ1USER    CMUSER
     C********             END                             END NE *BLANKS
     C********
     C********             MOVELQ1USER    CMIUSR
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********
     C********             Z-ADDDSTIME    CMTIME
     C********   SCDITP    IFEQ 'Y'
     C********   SCDITP    OREQ 'N'
     C********
     C****BEG*ADD *** 12/02/98 ****************************************
     C********   *IN38     IFEQ *ON
     C********   CTKY1     CHAINPCT01                60
     C**N60***             MOVE CTFL01    CMDIFL
     C********             ELSE                            *IN38 = ON
     C****END*ADD *** 12/02/98 ****************************************
     C********             MOVE SCDITP    CMDIFL
     C****BEG*ADD *** 12/02/98 ****************************************
     C********             ENDIF                           *IN38 = ON
     C****END*ADD *** 12/02/98 ****************************************
     C********             ELSE
     C****BEG*CHG *** 09/15/98 ****************************************
     C**********           MOVE ' '       CMDIFL
     C********   CTKY1     CHAINPCT01                60
     C**N60***             MOVE CTFL01    CMDIFL
     C****END*CHG *** 09/15/98 ****************************************
     C********             END
     C********                                             DFT LOC=DASD
     C********             MOVEL'D'       CMFL05           ADD 05/11/93
     C********
     C********             MOVE SCPGFL    CMFL02           ADD 03/20/92
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C********             Z-ADD*ZERO     CMDCDT
     C********             MOVELQ1PGM     CMIPGM           ADD 10/14/91
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C****BEGIN ADD *** 08/26/98 **************************************
     C********
     C**SET*DARTS DIARY TYPE BASED ON THE REMARK CODE.
     C********
     C********             MOVEL*BLANKS   SCCODE
     C********             MOVELSCDEPT    SCCODE
     C********             MOVE SCRMTY    SCCODE
     C********
     C****BEG*DEL *** 09/15/98 ****************************************
     C********** CTKY1     CHAINPCT01                99
     C**********
     C********** *IN99     IFEQ *OFF
     C**********
     C**********           MOVELCTFL01    CMDIFL
     C**********
     C**********           ENDIF
     C****END*DEL *** 09/15/98 ****************************************
     C********
     C****END*  ADD *** 08/26/98 **************************************
     C********
     C****GET*LAST SEQ# USED
     C********
     C********   CMKYA     SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   QQKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             END
     C********
     C********             WRITEPCM01
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QQKEY         Q#KEY            40
     C                   MOVEL(P)  SCCODE        Q#RMCD            5
     C     XXDEWR        IFEQ      'Y'
     C     Q#RMCD        CHAIN     PCT01                              60
     C   60              MOVE      *BLANKS       CTDEPG
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C     SCCOMM        IFEQ      *BLANKS
     C                   MOVEL(P)  DECOM1        Q#COMM          256
     C                   MOVE      DECOM2        Q#COM2           19
     C                   ELSE                                                   COMM = blanks
     C                   MOVEL(P)  SCCOMM        Q#COMM
     C                   MOVE      SCCOMM        Q#COM2
     C                   ENDIF                                                  COMM = blanks
     C                   MOVEL(P)  QQKEY         Q#FOLD           80
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   MOVEL     'N'           XXDEWR
     C                   ELSE                                                   DEWR = 'Y'
     C                   MOVEL(P)  SCDCID        Q#DCID
     C                   MOVEL     SCCOMM        Q#COMM
     C                   MOVE      SCCOMM        Q#COM2
     C                   MOVE      *BLANK        Q#FL01
     C                   MOVEL     SCDFLR        Q#FOLD
     C                   ENDIF                                                  DEWR = 'Y'
     C                   MOVEL(P)  SCCDAT        Q#CDAT            8
     C                   MOVEL(P)  QQPNAM        Q#PNAM           10
     C                   MOVE      ##SECL        Q#SECL            3
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL(P)  SCDUSR        Q#USER           10
     C                   ELSE                                                   DUSR = blanks
     C                   MOVEL     Q1USER        Q#USER
     C                   ENDIF                                                  DUSR = blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
     C     *IN38         IFEQ      *ON
     C     CTKY1         CHAIN     PCT01                              60
     C  N60              MOVE(P)   CTFL01        Q#DIFL            1
     C                   ELSE                                                   *IN38 = On
     C                   MOVE      SCDITP        Q#DIFL
     C                   ENDIF                                                  *IN38 = On
     C                   ELSE                                                   DITP = 'Y'
     C                   MOVE      *BLANK        Q#DIFL
     C     CTKY1         CHAIN     PCT01                              60
     C  N60              MOVE      CTFL01        Q#DIFL
     C                   ENDIF                                                  DITP = 'Y'
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  SCPGFL        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  ##SBYL        Q#SBYL            1
     C                   MOVEL(P)  SCAPID        Q#APID            2
     C                   MOVEL(P)  SCIDOC        Q#IDOC           12
     C                   MOVEL(P)  SCIQUE        Q#IQUE           10
     C                   MOVEL(P)  SCIMGF        Q#IMGF            1
     C                   MOVEL(P)  SCIPTH        Q#IPTH           10
     C                   MOVE      SCPIDX        Q#PIDX            5
     C                   MOVEL(P)  SCQPTY        Q#QPTY            1
     C                   MOVEL(P)  SCDCTP        Q#DCTP            5
     C                   MOVEL     SCCSID        Q#CSID            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   MOVE      Q#SEQ#        ##SEQ#
     C*** END ADD *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 02/10/99 **************************************
     C                   MOVEL     *BLANKS       DECOM1
     C                   MOVE      *BLANKS       DECOM2
     C*** END ADD *** 02/10/99 ****************************************
     C*
     C*** IF ADDITIONAL RMRKS ARE REQUESTED (SCARMK='Y') THEN CALL PROGRAM T
     C*** TO ADD THE REMARKS.
     C*
     C     SCARMK        IFEQ      'Y'
     C                   CALL      'SC014R'
     C                   PARM                    QQKEY
     C*** BEGIN CHG *** 04/25/00 **************************************
     C********             PARM           CMSEQ#
     C                   PARM                    ##SEQ#
     C*** END CHG *** 04/25/00 ****************************************
     C                   PARM                    QQPNAM                         12/18/91 ADD
     C                   END
     C*
     C                   MOVE      ##SEQ#        CMSEQ#
     C                   ENDSR
     C/EJECT
     C*
      *====================================================================*
      * * *   EDTS01  SUBROUTINE - EDIT SCREEN 1 FIELDS    E D T S 0 1   ***
      *====================================================================*
     C*
     C*
     C     EDTS01        BEGSR
     C*
     C* Date used must be valid.
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C     SRERR         CABEQ     '1'           STRT                     52
     C*** Begin add 05/28/03 ******************************************
     C*
     C* If DRFL20 = 'Y' SCCDAT must not be earlier than today
     C*
     C     SYKY          CHAIN     SDR01                              40
     C     *IN40         IFEQ      '0'
     C     DRFL20        IFEQ      'Y'
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENTDAT            8 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TODAYS            8 0
     C*
     C*
     C     ENTDAT        IFLT      TODAYS
     C                   MOVE      *ON           *IN88
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*** End   add 05/28/03 ******************************************
     C*
     C* Remark type must be entered and valid.
     C*
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     SCCODE        IFNE      *BLANKS
     C     SCCODE        CHAIN     STCTL1                             51
     C     *IN51         CABEQ     '1'           STRT
     C*** BEG ADD *** 08/26/98 ****************************************
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN13
     C                   GOTO      STRT
     C                   ENDIF                                                  CTSTAT = 'I'
     C*** END ADD *** 08/26/98 ****************************************
     C                   ELSE
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END                                                     SCCODE NE *BLANKS
     C*
     C* If remark is secured, the secuity level displayed on screen is moved
     C* into the field to be stored when record is made, otherwise, the
     C* remark code default security level is used.
     C*
     C*** BEGIN ADD *** 02/07/00 ***********************************************
     C     *IN56         IFEQ      *OFF
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C     CTDSLV        IFNE      *ZEROS
     C                   MOVE      'Y'           ##SBYL
     C                   MOVE      *ON           *IN56
     C                   ENDIF
     C                   MOVE      CTDSLV        SCSECL
     C                   ELSE
     C                   MOVE      *ZEROS        SCSECL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     SCSECL        ##SECL
     C*
     C***  Begin Add  ***  07/24/08  **************************************
     C     SVCODE        IFNE      *BLANK
     C     SVCODE        ANDNE     SCCODE
     C                   MOVEL     SCCODE        SVCODE
     C     SCDITP        IFNE      CTFL01
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***  End   Add  ***  07/24/08  **************************************
     C*
     C*** END   ADD *** 02/07/00 ***********************************************
     C*** BEGIN DEL *** 02/07/00 ***********************************************
     C********** *IN56     IFEQ '1'
     C********** SCSECL    ANDNE##SECL
     C**********           MOVELSCSECL    ##SECL
     C**********           ELSE
     C********** *IN56     IFEQ '0'
     C**********           MOVEL*ZEROS    SCSECL
     C**********           MOVELSCSECL    ##SECL
     C**********           END
     C**********           END
     C*** END   DEL *** 02/07/00 ***********************************************
     C*
     C*** BEG DEL *** 10/27/98 ****************************************
     C********** SCDITP    IFEQ 'Y'
     C*** END DEL *** 10/27/98 ****************************************
     C*
     C*** MUST ENTER A VALID USER NAME
     C*
     C                   MOVEL     SCDUSR        $$USER
     C     SLKY          CHAIN     PSL01                              58
     C     *IN58         CABEQ     '1'           STRT
     C*** BEG DEL *** 10/27/98 ****************************************
     C**********           END
     C*** END DEL *** 10/27/98 ****************************************
     C*
     C     SCDITP        IFNE      ' '
     C     SCDITP        IFNE      'Y'
     C     SCDITP        ANDNE     'N'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END SCDITP NE Y/N
     C                   ELSE                                                   ELSE SCDITP NE ' '
     C                   MOVE      CTFL01        SCDITP
     C                   END                                                    END SCDITP NE ' '
     C*
     C*  Check Data Entry Flag to see if Data Entry Program has been
     C*     called once already.
     C*
     C     XXDEFL        IFEQ      'N'
     C*  If DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
     C*     if it has an associated Data Entry Program ID.
     C*
     C                   MOVE      'N'           XXDEPG            1
     C     SYKY          CHAIN     SDR01                              40
     C     *IN40         IFEQ      '0'
     C     DRFL03        IFEQ      'Y'
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE      'Y'           XXDEPG
     C*
     C     SCDCID        IFNE      *BLANKS
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C*** BEGIN DLT *** 08/31/98 ******************************************
     C********** XXCDAT    IFLE XXDSDT
     C**********           MOVE *BLANKS   ##PGM
     C**********           CALL 'SRCHKPGM'
     C**********           PARM CTDEPG    ##PGM  10
     C**********           PARM           ##ERR   1
     C********** ##ERR     IFEQ *ON
     C**********           WRITESCR99
     C**********           WRITESCR01
     C**********           MOVE '1'       *IN47
     C**********           MOVE *BLANKS   Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF
     C**********
     C*  ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
     C*  (NOTE: It was necessary to break up the comment field of size
     C*   275 into DECOM1 and DECOM2 because the largest alpha-numeriC**********
     C*   value allowed for a parameter is 256.
     C**********
     C**********           MOVE *BLANKS   DEKEY
     C**********           MOVE *BLANKS   DECOM1
     C**********           MOVE *BLANKS   DECOM2
     C**********           MOVE *BLANKS   DERTCD
     C**********           MOVELQQKEY     DEKEY
     C**********
     C* MOVED THIS CODE UP ABOVE
     C**********           CALL CTDEPG                 37
     C**********           PARM           DEKEY
     C**********           PARM           DECOM1
     C**********           PARM           DECOM2
     C**********           PARM           DERTCD
     C**********
     C********** *IN37     IFEQ '1'
     C**********           EXSR ERROR
     C**********           WRITESCR99
     C**********           WRITESCR01
     C**********           MOVE *BLANKS   Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END
     C*** END DLT ***** 08/31/98 ******************************************
     C                   MOVE      'Y'           XXDEFL
     C*
     C*** BEGIN DLT *** 08/31/98 ******************************************
     C********** DERTCD    IFEQ 'A'
     C********** DERTCD    OREQ 'B'
     C**********           MOVE *BLANKS   Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** DERTCD    IFEQ *BLANKS
     C*** END DLT ***** 08/31/98 ******************************************
     C                   MOVE      'Y'           XXDEWR
     C*** BEGIN ADD *** 02/01/98 ***********************************************
     C     SCCOMM        IFEQ      *BLANKS
     C*** END ADD ***** 02/01/98 ***********************************************
     C                   MOVE      *BLANKS       SCCOMM
     C     DECOM1        IFEQ      *BLANK
     C     DECOM2        ANDEQ     *BLANK
     C                   MOVEL     CTDESC        SCCOMM
     C                   MOVEL     CTDESC        DECOM1
     C                   ELSE
     C                   MOVEL     DECOM1        SCCOMM
     C                   MOVE      DECOM2        SCCOMM
     C                   END
     C*** BEGIN ADD *** 02/01/98 ***********************************************
     C                   END
     C*** END ADD ***** 02/01/98 ***********************************************
     C*
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     XXCDAT        IFLE      XXDSDT
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN DLT *** 08/31/98 ******************************************
     C******************** END
     C*** END DLT ***** 08/31/98 ******************************************
     C*
     C                   END                                                    CTDEPG NE 0
     C                   END                                                    DRFL03 = 'Y'
     C                   END                                                    *IN40 = 0
     C                   END
     C*
     C*** Begin change* 11/13/00 ***********************************************
     C******     *IN99     IFEQ '0'                         NO EDIT IF IMAGE
     C******     XXDEFL    ANDEQ'N'
     C     *INKH         IFEQ      '0'                                          No edit if SCANGE
     C     XXDEFL        IFEQ      'N'
     C*** End   change* 11/13/00 ***********************************************
     C*
     C*** IF ENTERED, DOC ID MUST BE VALID (EXISTING)
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCDFLR        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD            1
     C*
     C     SCDCID        IFNE      *BLANK
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*** Must have a Default Folder entered for this function
     C     XXFOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     '1'           STRT                     54
     C*
     C                   END                                                    DCID <> *BLANK
     C*
     C                   END                                                    XXDEFL = N
     C*** Begin change* 11/13/00 ***********************************************
     C                   END                                                    *INKH = 0
     C*** End   change* 11/13/00 ***********************************************
     C*
     C     SCCOMM        CABEQ     *BLANKS       STRT                     53
     C*
     C     SCARMK        IFNE      'Y'
     C     SCARMK        CABNE     'N'           STRT                 4646
     C                   END
     C*
     C* PURGEABLE ITEM FLAG MUST BE 'Y', 'N', OR BLANK
     C*
     C     SCPGFL        IFNE      'Y'
     C     SCPGFL        ANDNE     'N'
     C     SCPGFL        ANDNE     *BLANK
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C* IF PURGEABLE ITEM FLAG IS = 'Y'
     C*
     C     SCPGFL        IFEQ      'Y'
     C*
     C     CTDPRG        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDITP        IFNE      'Y'
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C* IF PURGEABLE ITEM FLAG IS = *BLANKS
     C*
     C     SCPGFL        IFEQ      *BLANKS
     C*
     C     CTDPRG        IFNE      *ZEROS
     C     SCDITP        ANDEQ     'Y'
     C                   MOVE      'Y'           SCPGFL
     C                   ELSE
     C                   MOVE      'N'           SCPGFL
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   ERROR SUBROUTINE - EXCEPTION/ERROR SUBROUTINE           * * **
      *====================================================================*
     C     ERROR         BEGSR
     C*
     C*  IF FILE IS NOT FOUND IN USERS LIBL
     C*
     C     Q1STCD        IFEQ      211
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C*
     C                   MOVE      '1'           *IN59
     C                   ELSE
     C                   MOVE      '1'           *IN48
     C*
     C     Q2RECF        IFEQ      'SCR06'
     C     Q2RECF        OREQ      'SCR06C'
     C     Q2RECF        OREQ      'SCR06S'
     C*
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN6          XRRN6
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR06S
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*========START  ADD === 8/29/97 =====ISSIMG=========================*
      *====================================================================*
      * * *   DLTSCN SUBROUTINE - DELETE CM RCD FROM SCN    D L T S C N    *
      *====================================================================*
     C*
     C     DLTSCN        BEGSR
     C*
     C     CMKY$         DELETE    PCM0A                              60
     C*
     C     *IN60         IFEQ      *OFF
      *** Begin Add 11/25/03 ******************************************
     C                   MOVE      *BLANK        SCIMGF
      *** End   Add 11/25/03 ******************************************
     C*
     C*** VALID DELETE *********** ELSE ?????
     C*
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C/EJECT
     C*===========END ADD === 8/29/97 =====ISSIMG=========================*
     C/EJECT
     C*========START  ADD === 8/29/97 =====ISSIMG=========================*
      *====================================================================*
      * * *   UPDSCN SUBROUTINE - UPDATE CM RCD FROM SCN    U P D S C N    *
      *====================================================================*
     C*
     C     UPDSCN        BEGSR
     C*
     C     CMKY$         CHAIN     PCM0A                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     SCIDOC        CMIDOC
     C                   MOVEL     SCIDOC        CMDCID
     C                   MOVE      'Y'           CMIMGF
     C                   MOVEL     ZZFOLD        CMFOLD
     C                   UPDATE    PCM0A
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C/EJECT
     C*===========END ADD === 8/29/97 =====ISSIMG=========================*
      *** BEGIN ADD *** 11/19/99 ***********************************************
      *=========================================================================
      *  SRVDAR - Subroutine to check access to VDARTS and call VDARTS program
      *=========================================================================
     C     SRVDAR        BEGSR
      *
     C                   SELECT
     C     SLVDAD        WHENEQ    'D'
      ** Call the VDARTS program and goto TERM
     C                   CALL      VBDART        VBDPL
     C                   GOTO      TERM
      *
     C     SLVDAD        WHENEQ    ' '
      ** Deactivate the VDARTS Fkey
     C                   MOVE      ' '           XFVDTA
     C                   MOVE      *OFF          *IN02
      *
     C     SLVDAD        WHENEQ    'Y'
      ** Activate the VDARTS Fkey
     C                   MOVE      'Y'           XFVDTA
     C                   MOVE      *ON           *IN02
      *
     C                   ENDSL                                                  SLVDAD
      *
     C                   ENDSR                                                  SRVDAR
      *** END   ADD *** 11/19/99 ***********************************************
     OPAR01     E            RAR01
     OPCM0A     E            RCM0A
     OPHD01     E            RHD01
     OPH101     E            RH101
** FIELD NAMES
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
** UD FIELDS
UD01UD02UD03UD04UD05
** UN FIELDS
UN01UN02UN03UN04UN05
** UB FIELD NAMES (FB)
UB01UB02UB03UB04UB05UB06
**  CK - Command Keys - SCR02/SCR03
F3=Exit  F5=Work with rmks/docs  F24=More keys
F1=OV/400  F3=Exit  F5=Work with rmks/docs  F24=More keys
F3=Exit  F4=Summ Srch   F5=Work with rmks/docs  F24=More keys
F1=OV/400  F3=Exit  F4=Summ Srch  F5=Work with rmks/doc  F24=More keys
F6=Diary Items  F11=Add remark  F24=More keys
F6=Diary Items  F10=Search  F11=Add remark  F24=More keys
F6=Diary Items  F8=Image Srchs  F11=Add remark  F24=More keys
F6=Diary  F8=Image Srchs  F10=Search  F11=Add rmks  F24=More keys
F13=Data Scrn  F15=Date seq  F16=Type seq  F24=More keys
F13=Rmk Scrn  F15=Date seq  F16=Type seq  F24=More keys
F13=Data Scrn  F14=Summ Seq  F15=Date seq  F16=Type seq  F24=More keys
F13=Rmk Scrn   F14=Summ Seq  F15=Date seq  F16=Type seq  F24=More keys
F17=Doc ID Seq  F18=Seq entered  F19=Print Remarks  F24=More keys
F23=Visual DARTS  F24=More keys
**  CK1 - Command Keys - SCR01
F3=Exit  F5=Add office document  F24=More keys
F1=OV/400  F3=Exit  F5=Add office document  F24=More keys
F6=Display documents in a folder  F7=Chg Default folder  F24=More keys
F10=Search  F24=More keys
F8=Scan a Document  F10=Search  F24=More keys
F12=Previous  F20=Usr Exit  F23=Secure Remarks added  F24=More keys
F12=Previous  F20=Usr Exit  F23=Unsecure Remarks added  F24=More keys
