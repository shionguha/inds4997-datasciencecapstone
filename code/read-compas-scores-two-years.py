from sys import stdout
from csv import DictReader, DictWriter


class PeekyReader:
    def __init__(self, reader):
        self.peeked = None
        self.reader = reader

    def peek(self):
        if self.peeked is None:
            self.peeked = next(self.reader)
        return self.peeked

    def __iter__(self):
        return self

    def __next__(self):
        if self.peeked is not None:
            ret = self.peeked
            self.peeked = None
            return ret
        try:
            return next(self.reader)
        except StopIteration:
            self.peeked = None
            raise StopIteration


class Person:
    def __init__(self, reader):
        self.__rows = []
        self.__idx = reader.peek()['id']
        try:
            while reader.peek()['id'] == self.__idx:
                self.__rows.append(next(reader))
        except StopIteration:
            pass

    @property
    def lifetime(self):
        memo = 0
        for it in self.__rows:
            memo += int(it['end']) - int(it['start'])
        return memo

    @property
    def recidivist(self):
        return (self.__rows[0]['is_recid'] == "1" and
                self.lifetime <= 730)

    @property
    def violent_recidivist(self):
        return (self.__rows[0]['is_violent_recid'] == "1" and
                self.lifetime <= 730)

    @property
    def low(self):
        return self.__rows[0]['score_text'] == "Low"

    @property
    def high(self):
        return not self.low

    @property
    def low_med(self):
        return self.low or self.score == "Medium"

    @property
    def true_high(self):
        return self.score == "High"

    @property
    def vlow(self):
        return self.__rows[0]['v_score_text'] == "Low"

    @property
    def vhigh(self):
        return not self.vlow

    @property
    def vlow_med(self):
        return self.vlow or self.vscore == "Medium"

    @property
    def vtrue_high(self):
        return self.vscore == "High"

    @property
    def score(self):
        return self.__rows[0]['score_text']

    @property
    def vscore(self):
        return self.__rows[0]['v_score_text']

    @property
    def race(self):
        return self.__rows[0]['race']

    @property
    def valid(self):
        return (self.__rows[0]['is_recid'] != "-1" and
                (self.recidivist and self.lifetime <= 730) or
                self.lifetime > 730)

    @property
    def compas_felony(self):
        return 'F' in self.__rows[0]['c_charge_degree']

    @property
    def score_valid(self):
        return self.score in ["Low", "Medium", "High"]

    @property
    def vscore_valid(self):
        return self.vscore in ["Low", "Medium", "High"]

    @property
    def rows(self):
        return self.__rows


def count(fn, data):
    return len(list(filter(fn, list(data))))


def t(tn, fp, fn, tp):
    surv = tn + fp
    recid = tp + fn
    print("           \tLow\tHigh")
    print("Survived   \t%i\t%i\t%.2f" % (tn, fp, surv / (surv + recid)))
    print("Recidivated\t%i\t%i\t%.2f" % (fn, tp, recid / (surv + recid)))
    print("Total: %.2f" % (surv + recid))
    print("False positive rate: %.2f" % (fp / surv * 100))
    print("False negative rate: %.2f" % (fn / recid * 100))
    spec = tn / (tn + fp)
    sens = tp / (tp + fn)
    ppv = tp / (tp + fp)
    npv = tn / (tn + fn)
    prev = recid / (surv + recid)
    print("Specificity: %.2f" % spec)
    print("Sensitivity: %.2f" % sens)
    print("Prevalence: %.2f" % prev)
    print("PPV: %.2f" % ppv)
    print("NPV: %.2f" % npv)
    print("LR+: %.2f" % (sens / (1 - spec)))
    print("LR-: %.2f" % ((1-sens) / spec))


def table(recid, surv, prefix=''):
    tn = count(lambda i: getattr(i, prefix + 'low'), surv)
    fp = count(lambda i: getattr(i, prefix + 'high'), surv)
    fn = count(lambda i: getattr(i, prefix + 'low'), recid)
    tp = count(lambda i: getattr(i, prefix + 'high'), recid)
    t(tn, fp, fn, tp)


def hightable(recid, surv, prefix=''):
    tn = count(lambda i: getattr(i, prefix + 'low_med'), surv)
    fp = count(lambda i: getattr(i, prefix + 'true_high'), surv)
    fn = count(lambda i: getattr(i, prefix + 'low_med'), recid)
    tp = count(lambda i: getattr(i, prefix + 'true_high'), recid)
    t(tn, fp, fn, tp)


def vtable(recid, surv):
    table(recid, surv, prefix='v')


def vhightable(recid, surv):
    hightable(recid, surv, prefix='v')


def is_race(race):
    return lambda x: x.race == race

# given Person person and String attribute
# returns the attribute value of given person
# ex. get_cell(x, 'id') returns the id of the person x
def get_cell(person, attribute):
    return person.rows[0][attribute]

# given list of Persons people and String attribute
# returns the values used for that attribute
# ex. get_categories(people, 'age_cat') returns {'Less than 25', '25 - 45', 'Greater than 45'}
def get_categories(people, attribute):
    seta = set()
    for i in people:
        seta.add(get_cell(i, attribute))
    return seta

# returns json of a given person from people list
def get_person(person):
    return(person.rows[0])

def read_two_year_files():
    people = []
    with open("./data/compas-scores-two-years.csv") as f:
        reader = PeekyReader(DictReader(f))
        try:
            while True:
                p = Person(reader)
                if p.valid:
                    people.append(p)
        except StopIteration:
            pass
    return people

# prints all of the data
def print_two_year_files(people):
    for i in people:
        print(get_person(i))

if __name__ == "__main__":
    people = read_two_year_files()
    
    # dictionaries containing number of people considered 'Low', 'Medium', or 'High' risk based on age category
    less_than_25 = {'Low': 0, 'Medium': 0, 'High': 0}
    twentyfive_to_fortyfive = {'Low': 0, 'Medium': 0, 'High': 0}
    greater_than_45 = {'Low': 0, 'Medium': 0, 'High': 0}
    for i in people:
        age = get_cell(i, 'age_cat')
        score = get_cell(i, 'score_text')
        if(age == 'Less than 25'):
            less_than_25[score] += 1
        elif(age == '25 - 45'):
            twentyfive_to_fortyfive[score] += 1
        else:
            greater_than_45[score] += 1

    # prints percentage of low, medium, and high risks for people based on age

    print('Less than 25')
    print('Low:', less_than_25['Low'] / sum(less_than_25.values()))
    print('Medium:', less_than_25['Medium'] / sum(less_than_25.values()))
    print('High:', less_than_25['High'] / sum(less_than_25.values()), '\n')

    print('25 - 45')
    print('Low:', twentyfive_to_fortyfive['Low'] / sum(twentyfive_to_fortyfive.values()))
    print('Medium:', twentyfive_to_fortyfive['Medium'] / sum(twentyfive_to_fortyfive.values()))
    print('High:', twentyfive_to_fortyfive['High'] / sum(twentyfive_to_fortyfive.values()), '\n')

    print('Greater than 45')
    print('Low:', greater_than_45['Low'] / sum(greater_than_45.values()))
    print('Medium:', greater_than_45['Medium'] / sum(greater_than_45.values()))
    print('High:', greater_than_45['High'] / sum(greater_than_45.values()))