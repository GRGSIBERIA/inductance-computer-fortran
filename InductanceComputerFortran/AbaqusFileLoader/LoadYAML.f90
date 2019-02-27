    module YAML
    implicit none
    character*512 :: inpfile
    character*512 :: xyfile
    character*512 :: outfile
    
    contains
    
    ! ymlの設定をコロンで区切って読み込む手続き
    subroutine SeparateCoron(yamlFD, readData)
        integer, intent(in) :: yamlFD
        character*512, intent(in) :: readData
        
        integer i, coron, start, config
        coron = 0
        
        DO i = 1, 512
            IF (readData(i:i) == ":") THEN
                coron = i
            END IF
            
            IF (coron > 0) THEN
                IF (readData(i:i) /= " ") THEN  ! 実際の文字列が始まる場所を探索
                    start = i
                    GOTO 100
                END IF
            END IF
        END DO
        
100     continue
        
        ! 文字列の検索結果によってデータを代入
        IF (INDEX(readData(1:coron-1), "inpfile") > 0) THEN
            inpfile = readData(start:512)
        ELSE IF (INDEX(readData(1:coron-1), "xyfile") > 0) THEN
            xyfile = readData(start:512)
        ELSE IF (INDEX(readData(1:coron-1), "outfile") > 0) THEN
            outfile = readData(start:512)
        END IF
        
    end subroutine
    
    subroutine LoadYAML(yamlFD)
        integer, intent(in) :: yamlFD
        
        character*512, readData
        integer i
        
        OPEN (yamlFD, file="config.yml", status="old")
        
        DO i = 1, 3
            READ (yamlFD, *) readData
            CALL SeparateCoron(yamlFD, readData)
        END DO
        
        CLOSE (yamlFD)
    end subroutine
    end module