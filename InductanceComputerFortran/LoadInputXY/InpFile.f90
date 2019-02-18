
    module InpFile
    implicit none
    integer numofParts, numofOrigin
    integer, dimension(:), allocatable :: numofNodes        ! パートごとの接点数
    double precision, dimension(:,:), allocatable :: origin ! 原点座標
    
    contains
    
    subroutine ReadInpFile(fd)
        integer, intent(in) :: fd
        character*8 :: partSection
        integer countPart, nodeSection, nodeCount
        
        ! パート数をカウントする
        numofParts = 0
        
        DO
            read (fd, *, end=100) partSection
            
            IF (partSection(1:5) .EQ. "*Part") THEN
                numofParts = numofParts + 1
            END IF
        END DO
100     continue
        
        ALLOCATE (numofNodes(numofParts))
        
        ! パートごとに接点数をカウントする
        countPart = 0
        nodeSection = 0
        rewind (fd)
        
        DO
            read (fd, *, end=200) partSection
            
            IF (nodeSection .EQ. 1) THEN
                ! ElementがあったらNodeセクション終わり
                IF (partSection(1:8) .EQ. "*Element") THEN
                    nodeSection = 0
                END IF
                
                ! Nodeセクションに入っている
                numofNodes(countPart) = numofNodes(countPart) + 1
            ELSE
                ! Nodeセクションに入った
                IF (partSection(1:5) .EQ. "*Node") THEN
                    countPart = countPart + 1
                    nodeSection = 1
                END IF
            
                ! オプション付きのNodeセクションを数えるときがある
                IF (countPart > numofParts) THEN
                    GOTO 200
                END IF
            END IF

        END DO
200     continue

        ! 原点座標を入力する
        numofOrigin = SUM(numofNodes)
        ALLOCATE (origin(numofOrigin, 3))
        nodeSection = 0
        nodeCount = 1
        rewind (fd)
        
        DO
            IF (nodeSection == 1) THEN
                read (fd, *, end=300) partSection, origin(nodeCount, 1), origin(nodeCount, 2), origin(nodeCount, 3)
                
                if (partSection(1:8) == "*Element") THEN
                    nodeSection = 0
                ELSE
                    nodeCount = nodeCount + 1
                END IF
                
            ELSE
                read (fd, *, end=300) partSection
                
                IF (partSection(1:5) == "*Node") THEN
                    nodeSection = 1
                END IF
                    
            END  IF
            
        END DO
300     continue
        
    end subroutine
    
    subroutine DisposeInpModule()
        DEALLOCATE (numofNodes)
        DEALLOCATE (origin)
    end subroutine
    
    end module