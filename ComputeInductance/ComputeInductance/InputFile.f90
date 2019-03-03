    module InputFile
    use AssemblyClass
    implicit none
    
    contains
    
    subroutine SkipHeaders(inpFD, targetPart)
        implicit none
        integer, intent(in) :: inpFD
        character*64, intent(in) :: targetPart
        
        character*64 section
        character*64 name
        character*64 part
        
        DO
            READ (inpFD, *, end=2100) section, name, part
            
            IF (INDEX(section, "*Instance") > 0) THEN
                IF (INDEX(part, targetPart(1:LEN_TRIM(targetPart))) > 0) THEN
                    READ (inpFD, "()")  ! 見つかったので最初のノード情報のところへ飛ぶ
                    GOTO 2100
                END IF
            END IF
        END DO
2100    continue
    end subroutine
    
    type(Assembly) function LoadInput(inpFD, targetPart) result(assembly)
        implicit none
        integer, intent(in) :: inpFD
        character*64, intent(in) :: targetPart
        
        character*64 :: section
        character*64 :: sectionA
        character*64 :: sectionB
        character*64 :: sectionC
        integer numofNodes, countNodes, i
        
        numofNodes = 0
        
        ! 節点が記載されている場所まで飛ばす
        CALL SkipHeaders(inpFD, targetPart)
        
        ! 節点の数を数える
        DO
            READ (inpFD, *, end=2200) section
            
            IF (INDEX(section, "*Element") > 0) THEN ! Elementがあったら脱出
                GOTO 2200
            END IF
            
            numofNodes = numofNodes + 1
        END DO
2200    continue
        
        REWIND (inpFD)
        CALL SkipHeaders(inpFD, targetPart)
        
        ! 節点の情報を読み出す
        assembly = init_Assembly(numofNodes)
        countNodes = 0
        DO
            ! 数値ではなく文字列を区切りにしているので，文字列に一度代入しないと型変換エラーを起こす
            READ (inpFD, *, end=2400) section, sectionA, sectionB, sectionC
            
            IF (INDEX(section, "*Element") > 0) THEN
                GOTO 2400
            END IF
            
            countNodes = countNodes + 1
            
            ! 文字列を数値に変換する
            READ (section, *) assembly%nodeIds(countNodes)
            READ (sectionA, *) assembly%positions(countNodes, 1)
            READ (sectionB, *) assembly%positions(countNodes, 2)
            READ (sectionC, *) assembly%positions(countNodes, 3)
            
        END DO
2400    continue
        
    end function
    
    end module