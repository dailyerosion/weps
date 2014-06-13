!$Author$
!$Date$
!$Revision$
!$HeadURL$

! Module used to find the mean, variance, skew, and kurtosis of a series of numbers.
! Each time it is called with a new number, the statistics are updated.
! http://www.johndcook.com/standard_deviation.html
! Donald Knuth's Art of Computer Programming, Vol 2, page 232, 3rd edition.
! http://www.johndcook.com/skewness_kurtosis.html
module running_stats
  type statistics
    integer n
    real m1      ! mean (first moment)
    real m2      ! sum_of_squares (second moment)
    real m3      ! mean (third moment)
    real m4      ! mean (fourth moment)
    real variance
  end type statistics

contains

  ! initializes the statistics structure
  subroutine rs_initial( rs_stats )
    type(statistics) rs_stats
    rs_stats%n = 0
    rs_stats%m1 = 0
    rs_stats%m2 = 0
    rs_stats%m3 = 0
    rs_stats%m4 = 0
  end subroutine rs_initial

  ! add a new value to the running statistics
  subroutine rs_newnum( rs_stats, newnum )
    type(statistics) rs_stats
    real newnum

    integer n1
    real delta
    real delta_n
    real delta_n2
    real term1

    ! add value to running statistics (recurrence formulas)
    n1 = rs_stats%n
    rs_stats%n = rs_stats%n + 1
    delta = newnum - rs_stats%m1
    delta_n = delta / rs_stats%n
    delta_n2 = delta_n * delta_n
    term1 = delta * delta_n * n1
    rs_stats%m1 = rs_stats%m1 + delta_n
    rs_stats%m4 = rs_stats%m4 + term1*delta_n2*(rs_stats%n*rs_stats%n-3.0*rs_stats%n+3.0) &
                + 6.0*delta_n2*rs_stats%m2 - 4.0*delta_n*rs_stats%m3
    rs_stats%m3 = rs_stats%m3 + term1*delta_n*(rs_stats%n-2) - 3.0*delta_n*rs_stats%m2
    rs_stats%m2 = rs_stats%m2 + term1
  end subroutine rs_newnum

  function rs_count( rs_stats ) result(cnt)
    type(statistics) rs_stats
    integer cnt
    cnt = rs_stats%n
  end function rs_count

  function rs_mean( rs_stats ) result(mean)
    type(statistics) rs_stats
    real mean
    mean = rs_stats%m1
  end function rs_mean

  function rs_variance( rs_stats ) result(variance)
    type(statistics) rs_stats
    real variance
    variance = rs_stats%m2 / (rs_stats%n-1)
  end function rs_variance

  function rs_stddev( rs_stats ) result(stddev)
    type(statistics) rs_stats
    real stddev
    stddev = sqrt( rs_variance(rs_stats) )
  end function rs_stddev

  function rs_skewness( rs_stats ) result(skewness)
    type(statistics) rs_stats
    real skewness
    skewness = sqrt(real(rs_stats%n)) * rs_stats%m3 / (rs_stats%m2**1.5)
  end function rs_skewness

  function rs_kurtosis( rs_stats ) result(kurtosis)
    type(statistics) rs_stats
    real kurtosis
    kurtosis = (rs_stats%n * rs_stats%m4 / (rs_stats%m2*rs_stats%m2)) - 3.0
  end function rs_kurtosis

  ! given statistics for two sets, find the statistics for the combined set
  function rs_combine( rs_stats_a, rs_stats_b ) result(rs_combined)
    type(statistics) rs_stats_a
    type(statistics) rs_stats_b
    type(statistics) rs_combined

    real delta
    real delta2
    real delta3
    real delta4

    rs_combined%n = rs_stats_a%n + rs_stats_b%n

    delta = rs_stats_b%m1 - rs_stats_a%m1
    delta2 = delta * delta
    delta3 = delta * delta2
    delta4 = delta2 * delta2

    rs_combined%m1 = (rs_stats_a%n*rs_stats_a%m1 + rs_stats_b%n*rs_stats_b%m1) / rs_combined%n
    rs_combined%m2 = rs_stats_a%m2 + rs_stats_b%m2 + delta2*rs_stats_a%n*rs_stats_b%n / rs_combined%n
    rs_combined%m3 = rs_stats_a%m3 + rs_stats_b%m3 + delta3*rs_stats_a%n*rs_stats_b%n &
                   * (rs_stats_a%n-rs_stats_b%n) / (rs_combined%n*rs_combined%n) &
                   + 3.0*delta*(rs_stats_a%n*rs_stats_b%m2 + rs_stats_b%n*rs_stats_a%m2) / rs_combined%n
    rs_combined%m4 = rs_stats_a%m4 + rs_stats_b%m4 + delta4*rs_stats_a%n*rs_stats_b%n &
                   * (rs_stats_a%n*rs_stats_a%n-rs_stats_a%n*rs_stats_b%n+rs_stats_b%n*rs_stats_b%n) &
                   / (rs_combined%n*rs_combined%n*rs_combined%n) &
                   + 6.0*delta2*(rs_stats_a%n*rs_stats_a%n*rs_stats_b%m2+rs_stats_b%n*rs_stats_b%n*rs_stats_a%m2) &
                   / (rs_combined%n*rs_combined%n) &
                   + 4.0*delta*(rs_stats_a%n*rs_stats_b%m3 - rs_stats_b%n*rs_stats_a%m3) / rs_combined%n

  end function rs_combine

end module running_stats
