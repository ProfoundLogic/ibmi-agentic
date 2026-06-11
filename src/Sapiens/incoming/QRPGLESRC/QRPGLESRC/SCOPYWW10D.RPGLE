     D OL              S             80    DIM(20)                              Option lines
     D FL              S             80    DIM(20)                              Function lines
     D VO              S              1    DIM(99)                              Valid options
     D VF              S              1    DIM(24)                              Valid functions
     D OPTFNC          C                   CONST('SROPTFNC10')
     D ##OFD           DS          3500
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
     D DBFDS5          DS
     D  DBRRN5               397    400B 0
     D DBFDS6          DS
     D  DBRRN6               397    400B 0
     D DBFDS7          DS
     D  DBRRN7               397    400B 0
     D DBFDS8          DS
     D  DBRRN8               397    400B 0
     D DBFDS9          DS
     D  DBRRN9               397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*


     *******************************************************************
     ******** Add by Profound Logic for Dynamic Function Keys **********
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     *******************************************************************
     *******************************************************************

     D RCLCMD          C                   CONST('RCLRSC *CALLER')
     D PDSNAM          C                   CONST('Fiserv Insurance So-
     D                                     lutions, Inc.')
     D WWCONS          C                   CONST('Work with')
     D CDCONS          C                   CONST('Confirm Delete of')
     D SSCONS          C                   CONST('Subset')
