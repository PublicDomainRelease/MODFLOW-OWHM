      MODULE UTIL_MODULE
      USE CVT2STR, ONLY: NUM2STR
      IMPLICIT NONE
      PRIVATE:: NUM2STR
      CONTAINS
      !
      SUBROUTINE FILE_IO_ERROR(ERROR,UNIT,FNAME,LINE,INFILE,OUTPUT)
      ! ERROR  IS THE ASSOCIATED IOSTAT ERROR
      ! UNIT   IS THE FILE THAT IS BEING OPENED, READ FROM, OR WRITTEN TOO
      ! LINE   IS THE LINE THAT IS BEING PROCESSED EITHER FOR PARSING THE INPUT FILE, READING DATA, OR WRITING DATA
      ! INFILE IS THE FILE FROM WHICH LINE ORIGINATED FROM. IT CAN BE THE SAME FILE AS FNAME
      !
      INTEGER              :: ERROR
      INTEGER,     OPTIONAL:: UNIT,INFILE,OUTPUT
      CHARACTER(*),OPTIONAL::FNAME,LINE
      !
      INTEGER:: IU,IFILE,IOUT
      CHARACTER(500):: LN
      CHARACTER(1):: NL
      CHARACTER(:),ALLOCATABLE:: ERRMSG,FN,ERRLINE,INLINE
      LOGICAL::ISOPEN
      !
      NL=NEW_LINE(NL)
      IOUT=0
      IF(PRESENT(OUTPUT)) IOUT=OUTPUT
      !
      IF(PRESENT(INFILE)) THEN
          IFILE=INFILE
          INQUIRE(INFILE,NAME=LN)
          INLINE=TRIM(ADJUSTL(LN))
      ELSE
          IFILE=0
          INLINE='¿¿¿UNKOWN FILE???'
      END IF
      !
      IF(PRESENT(LINE))THEN
          ERRLINE=TRIM(ADJUSTL(LINE))
      ELSE
          ERRLINE='¿¿¿UNKOWN LINE???'
      END IF
      !
      IF(          PRESENT(FNAME).AND. .NOT. PRESENT(UNIT)) THEN
          FN=TRIM(ADJUSTL(FNAME))
          INQUIRE(FILE=FN,NUMBER=IU,OPENED=ISOPEN)
          IF(IU.EQ.-1) IU=0
      ELSEIF(.NOT. PRESENT(FNAME).AND.       PRESENT(UNIT))THEN
          IU=UNIT
          INQUIRE(IU,NAME=LN,OPENED=ISOPEN)
          FN=TRIM(ADJUSTL(LN))
      ELSEIF(.NOT. PRESENT(FNAME).AND. .NOT. PRESENT(UNIT))THEN
          IU=0
          FN=' '
          ISOPEN=.FALSE.
      ELSE
          IU=UNIT
          FN=TRIM(ADJUSTL(FNAME))
          INQUIRE(IU, OPENED=ISOPEN)
          IF(.NOT. ISOPEN) INQUIRE(FILE=FN,OPENED=ISOPEN)
      END IF
      !
      IF(IFILE.EQ.0) THEN
      !
      !
      IF(ISOPEN) THEN
          ERRMSG=NL//'FILE I/O ERROR:'
     +        //NL//'FOR FILE UNIT '//NUM2STR(IU)
     +        //NL//'WHICH IS ASSOCIATED WITH FILE '//FN
     +        //NL//'WHILE READING OR WRITING LINE "'//ERRLINE//'"'
     +        //NL//'AND HAS THE FOLLOWING IOSTAT ERROR: '
     +                    //NUM2STR(ERROR)
     +        //NL//NL//'NOTE THAT ERROR<0 MEANS END OF FILE ' 
     +        //'OR END OF RECORD CONDITION OCCURED.'
     +        //NL//'IF ERROR>0, THEN YOU HAVE TO LOOK UP THE '
     +        //'SPECIFIC ERROR CONDITION SPECIFIED BY THE COMPILER.'
      ELSEIF( .NOT. ISOPEN .AND. (IU.NE.0 .OR. FN.NE.' ') )THEN
          IF(IU.EQ.0) THEN
        ERRMSG=NL//'FILE I/O ERROR:'
     +        //NL//'FOR AN UNKNOWN FILE UNIT '
     +             //'[POSSIBLE FAILURE TO OPEN/FIND FILE]'
     +        //NL//'FOR THE REQUESTED FILE NAME '//FN
     +        //NL//'AND HAS THE FOLLOWING IOSTAT ERROR: '
     +             //NUM2STR(ERROR)   
          ELSE
        ERRMSG=NL//'FILE I/O ERROR:'
     +        //NL//'FOR FILE UNIT '//NUM2STR(IU)
     +             //'[POSSIBLE FAILURE TO OPEN/FIND FILE]'
     +        //NL//'WITH UNKNOWN FILE NAME'
     +        //NL//'AND HAS THE FOLLOWING IOSTAT ERROR: '
     +             //NUM2STR(ERROR)
          END IF          
      ELSE
        ERRMSG=NL//'FILE I/O ERROR:'
     +        //NL//'FOR AN UNKNOWN FILE UNIT AND FILE '
     +             //'[POSSIBLE FAILURE TO OPEN/FIND FILE]'
     +        //NL//'THE FOLLOWING IOSTAT ERROR CONDITION OCCURED: '
     +             //NUM2STR(ERROR)
     +       //NL//'FOR THE FOLLOWING LINE "'//ERRLINE//'"'
      END IF
      !
      !
      ELSE
      !
      !
      IF(ISOPEN) THEN
          ERRMSG=NL//'FILE I/O ERROR:'
     +       //NL//'FOR FILE UNIT '//NUM2STR(IU)
     +       //NL//'WHICH IS ASSOCIATED WITH FILE '//FN
     +       //NL//'WHILE UTILIZING THE FOLLOWING LINE: "'//ERRLINE//'"'
     +       //NL//'THAT IS ASSOCIATED WITH INPUT FILE: "'//INLINE//'"'
     +       //NL//'AND HAS THE FOLLOWING IOSTAT ERROR: '
     +                    //NUM2STR(ERROR)
     +        //NL//NL//'NOTE THAT ERROR<0 MEANS END OF FILE ' 
     +        //'OR END OF RECORD CONDITION OCCURED.'
     +        //NL//'IF ERROR>0, THEN YOU HAVE TO LOOK UP THE '
     +        //'SPECIFIC ERROR CONDITION SPECIFIED BY THE COMPILER.'
      ELSEIF( .NOT. ISOPEN .AND. (IU.NE.0 .OR. FN.NE.' ') )THEN
          IF(IU.EQ.0) THEN
        ERRMSG=NL//'FILE I/O ERROR:'
     +       //NL//'FOR AN UNKNOWN FILE UNIT '
     +             //'[POSSIBLE FAILURE TO OPEN/FIND FILE]'
     +       //NL//'FOR THE REQUESTED FILE NAME '//FN
     +       //NL//'AND HAS THE FOLLOWING IOSTAT ERROR: '
     +             //NUM2STR(ERROR)
     +       //NL//'WHILE UTILIZING THE FOLLOWING LINE: "'//ERRLINE//'"'
     +       //NL//'THAT IS ASSOCIATED WITH INPUT FILE: "'//INLINE//'"'
          ELSE
        ERRMSG=NL//'FILE I/O ERROR:'
     +       //NL//'FOR FILE UNIT '//NUM2STR(IU)
     +             //'[POSSIBLE FAILURE TO OPEN/FIND FILE]'
     +       //NL//'WITH UNKNOWN FILE NAME'
     +       //NL//'AND HAS THE FOLLOWING IOSTAT ERROR: '
     +             //NUM2STR(ERROR)
     +       //NL//'WHILE UTILIZING THE FOLLOWING LINE: "'//ERRLINE//'"'
     +       //NL//'THAT IS ASSOCIATED WITH INPUT FILE: "'//INLINE//'"'
          END IF          
      ELSE
        ERRMSG=NL//'FILE I/O ERROR:'
     +        //NL//'FOR AN UNKNOWN FILE UNIT AND FILE '
     +             //'[POSSIBLE FAILURE TO OPEN/FIND FILE]'
     +        //NL//'THE FOLLOWING IOSTAT ERROR CONDITION OCCURED: '
     +             //NUM2STR(ERROR)
     +       //NL//'WHILE UTILIZING THE FOLLOWING LINE: "'//ERRLINE//'"'
     +       //NL//'THAT IS ASSOCIATED WITH INPUT FILE: "'//INLINE//'"'
      END IF
      !
      !
      END IF
      !
      IF(IOUT.NE.0) WRITE(IOUT,'(A)') ERRMSG
      WRITE(*,'(A)') ERRMSG
      CALL USTOP(' ')
      !
      END SUBROUTINE
      !
      END MODULE