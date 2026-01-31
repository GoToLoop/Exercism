MODULE space_age; IMPLICIT NONE; CONTAINS
  ELEMENTAL DOUBLE PRECISION FUNCTION age_in_years(planet, seconds) RESULT(age)
    CHARACTER(*), INTENT(IN) :: planet; DOUBLE PRECISION, INTENT(IN) :: seconds

    DOUBLE PRECISION, PARAMETER :: SEC = 24*60*60*365.25d0, PERIODS(8) = (/ &
      .2408467d0, .61519726d0, 1d0, 1.8808158d0, 11.862615d0, 29.447498d0, &
      & 84.016846d0, 164.79132d0/)

    CHARACTER, PARAMETER :: PLANETS(8)*7 = (/"Mercury", "Venus  ", "Earth  ", &
      & "Mars   ", "Jupiter", "Saturn ", "Uranus ", "Neptune"/)

    age = seconds/(SEC*PERIODS(findloc(PLANETS, planet, 1))); END FUNCTION; END
