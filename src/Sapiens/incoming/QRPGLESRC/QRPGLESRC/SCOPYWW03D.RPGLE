     D OL              S             80    DIM(14)                              Option lines
     D FL              S             80    DIM(14)                              Function lines
     D VO              S              1    DIM(99)                              Valid options
     D VF              S              1    DIM(24)                              Valid functions
     D ER              S              2    DIM(99)                              Save indicator array
     D ##OFD           DS          3500
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D RCLCMD          C                   CONST('RCLRSC *CALLER')
     D PDSNAM          C                   CONST('StoneRiver')
     D WWCONS          C                   CONST('Work with')
     D CDCONS          C                   CONST('Confirm Delete of')

     *******************************************************************
     ******** Add by Profound Logic for Dynamic Function Keys **********
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     *******************************************************************
     *******************************************************************

