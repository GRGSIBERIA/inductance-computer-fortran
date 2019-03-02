    module InputFile
    implicit none
    
    contains
    
    subroutine LoadInput(inpFD, targetPart, numofNodes)
        implicit none
        integer, intent(in) :: inpFD
        character*64, intent(in) :: targetPart
        integer, intent(out) :: numofNodes
        
        character*64 section
        character*64 name
        character*64 part
        
        integer nodeId, skipFlag
        real, dimension(3) :: position
        
        numofNodes = 0
        skipFlag = 1
        
2000    continue
        
        ! 節点が記載されている場所まで飛ばす
        DO
            READ (inpFD, *, end=2200) section, name, part
            
            IF (INDEX(section, "*Instance") > 0) THEN
                IF (INDEX(part, targetPart(1:LEN_TRIM(targetPart))) > 0) THEN
                    READ (inpFD, "()")  ! 見つかったので最初のノード情報のところへ飛ぶ
                    GOTO 2100
                END IF
            END IF
        END DO
2100    continue
        
        IF (skipFlag == 1) THEN
            GOTO 2300   ! 節点の具体的な情報へ読み出すところへスキップする
        END IF
        
        ! 節点の数を数える
        DO
            READ (inpFD, "()", end=2200)
            
            IF (INDEX(section, "*Element") > 0) THEN ! Elementがあったら脱出
                GOTO 2200
            END IF
            
            numofNodes = numofNodes + 1
        END DO
2200    continue        
        
        REWIND (inpFD)
        skipFlag = 1
        GOTO 2000       ! もう一度，節点の直前まで処理させる
        
2300    continue
        
        ! 節点の情報を読み出す
        DO
            READ (inpFD, *, end=2400) nodeId, position
            
            IF (INDEX(section, "*Element") > 0) THEN
                GOTO 2400
            END IF
        END DO
2400    continue        
        
    end subroutine
    
    end module