    module YAML
    implicit none
    
    ! メモ
    ! 非透磁率
    ! 鉄: 5000
    ! ニッケル: 100～600
    
    ! gammaを決定するなら，
    ! 鉄を1としたとき
    ! ニッケルは0.1程度
    
    contains
    
    subroutine GetCoronId(readData, coron)
        character*512, intent(in) :: readData
        integer, intent(out) :: coron
        integer i
        
        DO i = 1, 512
            IF (readData(i:i) == ":") THEN  ! コロンの場所を特定する
                coron = i
                GOTO 200
            END IF
        END DO
200     continue
    end subroutine
    
    subroutine LoadYAML(yamlFD, inpFD, numofXYs, xyFDs, outFD)
        integer, intent(in) :: yamlFD
        integer, intent(in) :: inpFD
        integer, intent(in) :: outFD
        integer, intent(out) :: numofXYs
        integer, dimension(:), allocatable, intent(out) :: xyFDs
        
        character*512 :: inpfile
        character*512 :: outfile
        character, dimension(:,:), allocatable :: xyfiles
        
        character*512, readData
        integer i, numofLines, coronId, startStr, endStr, strFlag, fileId
        
        numofXYs = 0
        
        OPEN (yamlFD, file="config.yml", status="old")
        
        ! レポートの数を数える
        DO
            READ (yamlFD, *, end=300) readData
            
            CALL GetCoronId(readData, coronId)
            
            IF (INDEX(readData(1:coronId-1), "xyfile") > 0) THEN
                numofXYs = numofXYs + 1
            END IF
        END DO
300     continue
        
        ALLOCATE (xyfiles(numofXYs, 256))   ! XYレポートを確保
        
        ! 先頭に戻ってデータを入れる
        REWIND (yamlFD)
        fileId = 1
        
        DO
            READ (yamlFD, *, end=400) readData
            
            ! 文字列の範囲を特定する
            CALL GetCoronId(readData, coronId)
            startStr = coronId + 1
            DO i = startStr, 512
                IF (strFlag == 0) THEN
                    IF (readData(i:i) /= " ") THEN
                        startStr = i
                        strFlag = 1
                    END IF
                ELSE
                    IF (readData(i:i) == " ") THEN
                        endStr = i - 1
                        GOTO 500
                    END IF
                    
                END IF
            END DO
500         continue
            
            ! 各ファイルのパスを収集する
            IF (INDEX(readData(1:coronId-1), "xyfile") > 0) THEN
                xyfiles(fileId,:) = readData(startStr:endStr)
            ELSE IF (INDEX(readData(1:coronId-1), "outfile") > 0) THEN
                outfile = readData(startStr:endStr)
            ELSE IF (INDEX(readData(1:coronId-1), "inpfile") > 0) THEN
                inpfile = readData(startStr:endStr)
            END IF
            
        END DO
400     continue
        
        ! ファイルを開く
        OPEN (inpFD, file=inpfile, status="old")
        OPEN (outFD, file=outfile, status="replace")
        DO i = 1, numofXYs
            OPEN (xyFDs(i), file=xyfiles(i,:), status="old")
        END DO
        
        DEALLOCATE (xyfiles)
        CLOSE (yamlFD)
    end subroutine
    
    end module