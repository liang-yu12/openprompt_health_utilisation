from databuilder.ehrql import days
from databuilder.tables.beta.tpp import appointments

def add_visits(dataset, from_date, num_months):
    # Number of GP visits within `num_months` of `from_date`
    num_visits = appointments \
        .take((appointments.start_date >= from_date) &
              (appointments.start_date <= (from_date + days(num_months * 30)))) \
        .count_for_patient()
    setattr(dataset, f"gp_visit_m{num_months}", num_visits)
