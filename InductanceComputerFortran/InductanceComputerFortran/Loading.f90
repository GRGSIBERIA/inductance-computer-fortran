module Loading
    implicit none
    contains
    
    function read_vector(FD, time_count, count)
        integer, intent(in) :: time_count, count
        integer, intent(in) :: FD
        double precision, dimension(time_count, count, 3) :: read_vector
        integer i, j, k
        
        DO i = 1, time_count
            DO k = 1, 3
                DO j = 1, count
                    READ (FD, *) read_vector(i, j, k)
                END DO
            END DO
        END DO
    end function
    
    function load_wire_position(path, time_count)
        character, dimension(256), intent(in) :: path
        integer, intent(in) :: time_count
        double precision, dimension(:,:,:) :: load_wire_position
        integer, parameter :: FD = 18
        integer wire_num, i, j
        
        OPEN (FD, file=path, status="old")
        READ (FD, *) wire_num
        
        load_wire_position = read_vector(FD, time_count, wire_num)
            
        CLOSE(FD)
    end function
    
    function load_coil_position(path, time_count)
        character, dimension(256), intent(in) :: path
        integer, intent(in) :: time_count
        integer, parameter :: FD = 18
        double precision, dimension(:,:,3) :: load_coil_position
        integer coil_num, i, j
        
        OPEN (FD, file=path, status="old")
        READ (FD, *) coil_num
        
        load_coil_position = read_vector(FD, time_count, coil_num)
        
        CLOSE(FD)
    end function
    
    function read_scalar_one(FD, time_num)
        integer, intent(in) :: FD, time_num
        double precision, dimension(time_num) :: read_scalar_one
        integer i
        
        DO i = 1, time_num
            READ (FD, *) read_scalar_one(i)
        END DO
    end function
    
    function load_time(path)
        character, dimension(256), intent(in) :: path
        integer, parameter :: FD = 18
        integer time_num, i
        double precision, dimension(:) :: load_time
        
        OPEN (FD, file=path, status="old")
        READ (FD, *) time_num
        
        load_time = read_scalar_one(FD, time_num)
        
        CLOSE(FD)
    end function
end module