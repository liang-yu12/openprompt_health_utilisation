from databuilder.ehrql import Dataset
from databuilder.codes import REGISTRY, Codelist, codelist_from_csv
from databuilder.tables.beta.tpp import (clinical_events)
import codelists_ehrql


# combine long covid codelists
lc_dx = clinical_events.snomedct_code.is_in(lc_codelists_combined).sort_by(clinical_events.date).first_for_patient()

# generate dummy tidy dataset
dataset = Dataset()
dataset.set_population(lc_dx)
dataset.dx_date = clinical_events.date