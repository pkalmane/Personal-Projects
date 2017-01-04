'========================================================================================
'clsDate.inc
'Creating a class that gives different type of date strings
'========================================================================================

MACRO trimstr(l1) =TRIM$(STR$(l1))

'========================================================================================
CLASS clsDate


CLASS METHOD Destroy
    'DCM("Destroy")
END METHOD
'-------------------------------------------------------
    INTERFACE DateInterface
    INHERIT IUNKNOWN

'-------------------------------------------------------
   METHOD sddmmyyyyS(sDate AS STRING) AS STRING
    LOCAL s1 AS STRING

    s1=sDate
    s1=MID$(s1,4,2)+"-"+LEFT$(s1,2)+"-"+RIGHT$(s1,4)
    METHOD=s1
   END METHOD

'-------------------------------------------------------
   METHOD sddmmyyyy() AS STRING
    LOCAL s1 AS STRING

    s1=DATE$
    s1=MID$(s1,4,2)+"-"+LEFT$(s1,2)+"-"+RIGHT$(s1,4)
    METHOD=s1
   END METHOD

'-------------------------------------------------------
   METHOD sddmmyyyyhhmmss() AS STRING
    LOCAL s1 AS STRING

    s1=DATE$
    s1=MID$(s1,4,2)+"-"+LEFT$(s1,2)+"-"+RIGHT$(s1,4)+","+TIME$
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD syyyymmdd() AS STRING
    LOCAL s1 AS STRING

    s1=DATE$
    s1=RIGHT$(s1,4)+"-"+LEFT$(s1,2)+"-"+MID$(s1,4,2)
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD syyyymmddhhmmss() AS STRING
    LOCAL s1 ,s2 AS STRING

    s1=DATE$
    s1=RIGHT$(s1,4)+"-"+LEFT$(s1,2)+"-"+MID$(s1,4,2)
    s2=TIME$
    REPLACE ANY ":" WITH "-" IN s2
    s1=s1+"-"+s2
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD ConvertSecondsToHHMMSS(lValueSeconds AS LONG) AS STRING
    LOCAL s1 ,s2 ,s3 AS STRING
    LOCAL l1,lHour,lMin, lSec AS LONG
    l1=INT(lValueSeconds/60)
    lSec=lValueSeconds MOD 60
    lHour=INT(l1/60)
    lMin=l1 MOD 60

    s1="00"+TRIMSTR(lHour)
    s1=RIGHT$(s1,2)
    s2="00"+TRIMSTR(lMin)
    s2=RIGHT$(s2,2)
    s3="00"+TRIMSTR(lSec)
    s3=RIGHT$(s3,2)
    s1=s1+":"+s2+":"+s3
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD ConvertSecondsToHHHHMM(lValueSeconds AS LONG) AS STRING
    LOCAL s1 ,s2 AS STRING
    LOCAL l1,lHour,lMin AS LONG
    l1=INT(lValueSeconds/60)
    lHour=INT(l1/60)
    lMin=l1 MOD 60
    IF (lValueSeconds-l1*60)>0 THEN INCR lMin

    s1="0000"+TRIMSTR(lHour)
    s1=RIGHT$(s1,4)
    s2="00"+TRIMSTR(lMin)
    s2=RIGHT$(s2,2)
    s1=s1+":"+s2
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD ConvertHoursToHHHH(lValueHours AS LONG) AS STRING
    LOCAL s1 ,s2 AS STRING
    LOCAL l1,lHour,lMin AS LONG
    lHour=lvalueHours

    s1="0000"+TRIMSTR(lHour)
    s1=RIGHT$(s1,4)
    METHOD=s1
   END METHOD

'-------------------------------------------------------
   METHOD ConvertNumberYYYYMMDD(lNumber AS LONG) AS STRING
    LOCAL s1 ,s2 AS STRING
    s1=TRIM$(STR$(lNumber))
    s2=RIGHT$(s1,2)+"-"+MID$(s1,5,2)+"-"+LEFT$(s1,4)
    METHOD=s2
   END METHOD

'-------------------------------------------------------
  END INTERFACE
'-------------------------------------------------------
END CLASS
