    module Report
    implicit none
    
    contains
    subroutine LoadReport(xyFD, numofTimes)
        integer, intent(in) :: xyFD, numofTimes
        
        character*64 header, capture, discard
        integer nodeNumber
        integer displaceNumber
        
        READ (xyFD, "()")   ! 先頭行のスキップ
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 情報の数だけループを繰り返す
        ! まだここは未実装
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        ! 1行目には様々な情報が含まれている
        READ (xyFD, "(A)", end=3000) header
        
        ! 2行目以降は連続的に読み込む
        DO
            READ (xyFD, "(A)", end=3000) capture
            IF (INDEX(capture, "X  ") > 0) THEN
                GOTO 3000
            END IF
        END DO
3000    continue
        
        ! ヘッダの中で必要な情報だけ読み取る
        READ (header(INDEX(header, "U:U")+3:), *) displaceNumber
        READ (capture(INDEX(capture, "N: ")+3:), "(I)") nodeNumber
        
        READ (xyFD, "()")   ! 1行読み捨てる
        
        ! 時系列データが格納されている
        
    end subroutine
    
    end module