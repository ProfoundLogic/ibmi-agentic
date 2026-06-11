      *=====================================================================
      * Number of input variables
      *=====================================================================
     DnbrVars          s             10i 0
      *
      *=====================================================================
      * Saved query string
      *=====================================================================
      * Saved query string
     Dsavedquerystring...
     D                 s          32767    varying
      *
      *=====================================================================
      * Constant for updHTMLvar subprocedure
      *=====================================================================
     D initHTMLVars    c                   '0'
      *
      *=====================================================================
      * For returning environment variables
      *=====================================================================
     D Remote_ide      S            512A
     D Remote_use      S            512A
     D Q_String        S            512A
     D S_Software      S            256A                                        File containing
     D S_Name          S            256A                                        File containing
     D S_Protocol      S            256A                                        File containing
     D G_Interf        S            256A                                        File containing
     D S_Port          S            256A                                        File containing
     D R_Method        S            256A                                        File containing
     D P_Info          S            256A                                        File containing
     D P_Trans         S            256A                                        File containing
     D Script_N        S            256A                                        File containing
     D R_Host          S            256A                                        File containing
     D R_Addr          S            256A                                        File containing
     D HTTP_Acc        S            256A                                        File containing
     D HTTP_Refer      S            256A                                        File containing
     D HTTP_UserA      S            256A                                        File containing
     D C_Type          S            256A                                        File containing
     D C_Length        S            256A                                        File containing
     D IBM_CCSID       S            256A                                        File containing
     D AUTH_TYPE       S            256A                                        File containing
     D CGI_A_CCS       S            256A                                        File containing
     D CGI_MODE        S            256A                                        File containing
     D CGI_E_CCS       S            256A                                        File containing
      *=====================================================================
      * Variable for countp subprocedure
      *=====================================================================
     D counter         S             10i 0                                      Returned counter
      *=====================================================================
      * For program status data structure and program status subroutine
      *=====================================================================
     D psds           sds                                                       Pgm status DS
     D   psdsdata                   429                                         The data
     D pssrswitch      s              1    inz(*off)                            switch for pssr
     D wrotetop        s              1    inz(*off)                            Whthr top sec wrtn
      *
      *=====================================================================
      * Variables for retrieving cgi variable counts and numbers
      *=====================================================================
     D VarCount        s             10i 0                                      cgivarval
     D VarOcc          s             10i 0                                      occurence
      *
      * Return code
     D rc              s             10i 0 inz(0)                               return code
      *
     D oses            s            300                                         oper systems
      *
      *=====================================================================
      * For handling persistence
      *=====================================================================
     Dsessionid        s             15
     Dpathinfo         s            250
     Dpathinfolen      s             10i 0
     Dtimeout          s              5u 0 inz(3)                               min for httimeout hd
      *=====================================================================
      * Program timing variables
      *=====================================================================
     D timedata1       s               z
     D timedata2       s               z
     D ms              s             10i 0
     D sec             s             10p 3
      *
      *=====================================================================
      * Indicators for chknbr subprocedure
      *=====================================================================
     D ChkNbrInds      ds
     D  Indicators                     n   dim(7)
      *
      *=====================================================================
      * Initialization complete switch
      *=====================================================================
     D InitComplete    s               n
      *
      *=====================================================================
      * Some nice fields to help us through from lower to upper case character conversion
      *=====================================================================
     D lw              C                   CONST('abcdefghijklmnopqrstuvwxyz')
     D up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      *=====================================================================
      * Program timing variables
      *=====================================================================
     D TimeStart       s               z
     D TimeEnd         s               z
     D MsElaps         s             10i 0
     D SecElaps        s             10p 3
