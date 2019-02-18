
    module InpFile
    implicit none
    integer numofParts, numofOrigin
    integer, dimension(:), allocatable :: numofNodes        ! �p�[�g���Ƃ̐ړ_��
    double precision, dimension(:,:), allocatable :: origin ! ���_���W
    
    contains
    
    subroutine ReadInpFile(fd)
        integer, intent(in) :: fd
        character*8 :: partSection
        integer countPart, nodeSection, nodeCount
        
        ! �p�[�g�����J�E���g����
        numofParts = 0
        
        DO
            read (fd, *, end=100) partSection
            
            IF (partSection(1:5) .EQ. "*Part") THEN
                numofParts = numofParts + 1
            END IF
        END DO
100     continue
        
        ALLOCATE (numofNodes(numofParts))
        
        ! �p�[�g���Ƃɐړ_�����J�E���g����
        countPart = 0
        nodeSection = 0
        rewind (fd)
        
        DO
            read (fd, *, end=200) partSection
            
            IF (nodeSection .EQ. 1) THEN
                ! Element����������Node�Z�N�V�����I���
                IF (partSection(1:8) .EQ. "*Element") THEN
                    nodeSection = 0
                END IF
                
                ! Node�Z�N�V�����ɓ����Ă���
                numofNodes(countPart) = numofNodes(countPart) + 1
            ELSE
                ! Node�Z�N�V�����ɓ�����
                IF (partSection(1:5) .EQ. "*Node") THEN
                    countPart = countPart + 1
                    nodeSection = 1
                END IF
            
                ! �I�v�V�����t����Node�Z�N�V�����𐔂���Ƃ�������
                IF (countPart > numofParts) THEN
                    GOTO 200
                END IF
            END IF

        END DO
200     continue

        ! ���_���W����͂���
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