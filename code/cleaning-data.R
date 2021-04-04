library(dplyr)
library(lubridate)

setwd('~/Documents/inds4997-datasciencecapstone')
data <- read.csv('./data/compas-scores.csv')

# Remove unnecessary columns
data <- select(data, c(sex, age, age_cat, race, juv_fel_count, juv_misd_count, juv_other_count, priors_count, days_b_screening_arrest, c_days_from_compas, c_jail_in, c_jail_out, c_charge_degree, c_charge_desc, is_recid, decile_score, score_text))

# Remove N/A's found in score_text
data <- data %>% 
  filter(!score_text == "N/A")

# Order Score Text for graph processing later
data$score_text <- factor(data$score_text, 
                          order = TRUE, 
                          levels = c("Low", "Medium", "High"))

# Lists containing whether crimes are either violent or non-violent
# General rule is that committing or threatning violence to a human is considered violent
# ex. Carrying a weapon illegally is not violent but armed robbery is
violent <- list('Assault', 'Armed Carjacking', 'Sex Batt Faml/Cust Vict 12-17Y', 'Battery', 'Battery on a Person Over 65',
                'Battery on Law Enforc Officer', 'Sexual Battery Victim 12 Yrs +', 'Aggravated Assault w/Firearm',
                'Felony Battery (Dom Strang)', 'Burglary Dwelling Armed', 'Resist Officer w/Violence', 'Aggrav Battery w/Deadly Weapon',
                'Murder in the First Degree', 'Aggravated Battery / Pregnant', 'Kidnapping', 'Agg Batt W/Arm S/B/I 25 Min/Ma',
                'Stalking (Aggravated)', 'Attempt Murder in the First Degree', 'Battery Upon Detainee', 'Assault On Law Enforc Officer',
                'Aggravated Battery On 65/Older', 'Lewd/Lasc Battery Pers 12+/<16', 'Throw Missile Into Pub/Priv Dw',
                'Harm Public Servant Or Family', 'Robbery', 'Aggrav Child Abuse-Agg Battery', 'Shoot/Throw Into Vehicle',
                'Sex Battery Deft 18+/Vict 11-', 'Agg Battery Grt/Bod/Harm', 'Burglary Structure Assault/Batt', 'Throw Deadly Missile Into Veh',
                'Aggravated Assault W/dead Weap', 'Murder in 2nd Degree', 'Sexual Battery / Vict 12 Yrs +', 'Felony Batt(Great Bodily Harm)',
                'Home Invasion Robbery', 'Child Abuse', 'Strong Armed  Robbery', 'Threaten Throw Destruct Device', 'Burgl Dwel/Struct/Convey Armed',
                'Aggravated Battery', 'Aggrav Stalking After Injunctn', 'Felony Battery', 'Burglary Dwelling Assault/Batt',
                'Trespass Property w/Dang Weap', 'Robbery Sudd Snatch w/Weapon', 'Armed Kidnapping', 'Robbery Firearm Wearing Mask',
                'Burglary Conveyance Assault/Bat', 'Robbery / Weapon', 'Robbery W/Firearm', 'Burglary Dwelling Occupied',
                'Throw In Occupied Dwell', 'Agg Assault W/int Com Fel Dome', 'Felony Battery w/Prior Convict', 'Making An Affray/Riot',
                'Attempted Robbery Deadly Weapo', 'Threat Public Servant', 'Armed False Imprisonment', 'Aggravated Assault W/Dead Weap',
                'Domestic Battery By Stran', 'Murder 2nd Degree W/Deadly Wep', 'Burglary With Assault/battery', 'Lewd or Lascivious Molestation',
                'Carjacking with a Firearm', 'Aggravated Assault W/o Firearm', 'Attempt Armed Burglary Dwell', 'Obstruct Officer W/Violence',
                'Sexual Performance by a Child', 'Felony DUI (level 3)', 'Aggravated Battery (Firearm/Actual Possession)', 'Battery On Parking Enfor Speci',
                'Aggravated Assault', 'Manslaughter W/Weapon/Firearm', 'Att Burgl Conv Occp', 'Burglary Structure Armed', 'Battery Emergency Care Provide',
                'Criminal Attempt 2nd Deg Felon', 'Neglect/Abuse Elderly Person', 'Trespass Structure w/Dang Weap', 'Cruelty Toward Child',
                'Aggrav Child Abuse-Causes Harm', 'Burglary Conveyance Armed', 'Agg Assault Law Enforc Officer',
                'Attempt Burglary (Struct)', 'Aggr Child Abuse-Torture,Punish', 'Robbery W/Deadly Weapon', 'Assault/Battery On An Official',
                'Kidnapping / Domestic Violence', 'Battery On A Person Over 65', 'Arson in the First Degree',
                'Shoot In Occupied Dwell', 'Possess/Use Weapon 1 Deg Felon', 'Abuse Without Great Harm', 'Battery On Fire Fighter',
                'Aggravated Battery (Firearm)', 'Murder In 2nd Degree W/firearm', 'Batt/on a Specified Employee', 'Crlty Twrd Child Urge Oth Act',
                'Attempted Robbery  Weapon', 'Aggravated Manslaughter Child', 'Lewd/Lasciv Molest Elder Persn', 'Burglary Assault/Battery Armed',
                'Attempted Strongarm Robbery', 'Shoot Into Vehicle', 'Battery Spouse Or Girlfriend', 'Agg Abuse Elderlly/Disabled Adult',
                'Assault Law Enforcement Officer', 'Burglary', 'Unlawful Sexual Activity', 'Armed Trafficking in Cannabis',
                'Agg Battery Cause Bodily', 'Shoot Into Aircraft', 'Attempted Aggravated Battery', 'Attempt Sexual Batt / Vict 12+')
non_violent <- list('Obstruct Fire Equipment', 'Poss/Sel/Del Cocaine 1000FT Chur', 'Poss3,4 Methylenedioxymethcath', 'Indecent Exposure',
                    'Contribute Delinq/Depnd of Minor', 'POSSESS FIELD BOX WITH REGISTERED MARK', 'Use Lost Or Stolen Credit Card',
                    'False Motor Veh Insurance Card', 'Prowling/Loitering', 'Poss F/Arm Delinq', 'Open Carrying Of Weapon',
                    'Trespass Struct/Convey Occupy', 'Unauth C/P/S Sounds>1000/Audio', 'DUI Level 0.15 Or Minor In Veh',
                    'Grand Theft on 65 Yr or Older', 'Leave Acc/Attend Veh/More $50', 'Permit Unauthorizd Person Drv',
                    'Restraining Order Dating Viol', 'Poss of Firearm by Convic Felo', 'Possession of Methadone', 'Possess w/I/Utter Forged Bills',
                    'Loitering/Prowling', 'Expired DL More Than 6 Months', 'Poss Anti-Shoplifting Device', 'Harass Witness/Victim/Information',
                    'Make False Affidavit For DL', 'Fail To Obey Police Officer', 'Culpable Negligence', 'DWLS Canceled Disqul 1st Off',
                    'Grand Theft in the 3rd Degree', 'Poss Firearm Commission Felony', 'False Reports', 'Susp Drivers Lic 1st Offense',
                    'Driving W/Lic Susp Habitual', 'Trespass Other Struct/Convey', 'Carrying Concealed Firearm',
                    'Driving while DL Susp / Financial Resp', 'Eng/Bus/Contract W/O License', 'False Imprisonment', 'Poss Pyrrolidinovalerophenone',
                    'Utilizing Juvenile to Deliver', 'Fraud Use/Persnl ID Info/Deceased', 'Pos Cannabis W/Intent Sel/Del', 'Felony Petit Theft',
                    'Destroy Damage Alter Elec Monitor Equip', 'Traffick Oxycodone    14g><28g', 'Possess Cannabis 1000FTSch',
                    'Child Neglect/Delinquency', 'Tamper With Victim', 'Temporary Tag Violation', 'Del Pyrrolidinovalerophenone',
                    'Enter City Park/ Prohibited Hrs', 'Sale/Del Cocaine Child Care Fac', 'Possess Countrfeit Credit Card', 'Giving False Report',
                    'Grand Theft In The 3Rd Degree', 'Poss Counterfeit Payment Inst', 'Corrupt Public Servant', 'Criminal Mischief Damage <$200',
                    'Sell Methamphetamine', 'Felony Driving While Lic Suspd', 'Possession of Morphine', 'No Valid DL / Non Resident',
                    'Trafficking In Cocaine 200-400', 'Racing On Highway', 'Tampering with a Victim', 'Sell Cocaine 1000FT School', 'Unlawful Use Of License',
                    'Mandatory Susp Possess Alcohol', 'Unlaw Pos of Prson ID of another', 'Cash Item w/Intent to Defraud', 'Introduce Contraband Into Jail',
                    'Renting For Prostitution', 'Viol Prot Injunc Repeat Viol', 'Loiter Solicit Act Prostitute', 'Fraud Obtain/ Use Disable Permit',
                    'No Court Susp Petit Theft', 'Deliver 3,4 Methylenediox', 'Fleeing or Eluding a LEO', 'Interfere With K9/Horses Duties',
                    'Falsely Personating Officer', 'Burglary Conveyance Unoccup', 'Contempt Of Court', 'Possession of Hydrocodone',
                    'Violation of Injunction Order/Stalking/Cyberstalking', 'Drink Near Licensed Establishm', 'Viol Injunction Protect Dom Violence',
                    'Conspire Traffic Illegal Drugs', 'Take Copper Other Metal Intrf/Damg Utility', 'Unlaw LicTag/Sticker Attach', 
                    'Deliver Cocaine 1000FT Park', 'Possession Of Clonazepam', 'Poss Wep Conv Felon', 'Possession of Hydromorphone',
                    'Fraudulent Use Credit Card', 'Sell/Man/Del Pos/w/int Heroin', 'Traffick Hydrocodone   4g><14g', 'Burglary Structure Unoccup',
                    'Possession Of Lorazepam', 'Possession of Ethylone', 'Solicit ProstitutionViolation', 'Trespass Struct/Conveyance',
                    'Transport Prostitution', 'Stalking', 'Attempt Crim Use of Personal ID Info', 'Depriv LEO of Protect/Communic',
                    'Crimin Mischief Damage $1000+', 'Ped Obstruct Traf/No Permit Sol', 'Prostitution', 'Robbery Sudd Snatch No Weapon',
                    'Carrying A Concealed Weapon', 'Traffick Methampheta 28g><200g', 'Reckless Driving', 'DUI- Enhanced',
                    'Possession Of Carisoprodol', 'Lewd/Assignation/Prostitution', 'Open Container Of Alcoholic Bev',
                    'Sell/Man/Del/Pos/W/Int Methado', 'Possession Of Alcohol Under 21', 'Fail Register Vehicle', 'Neglect Child / No Bodily Harm',
                    'Theft/To Deprive', 'Criminal Mischief>$200<$1000', 'Unauth Poss ID Card or DL', 'Obstuct By Solicitation', 'Possession Of Paraphernalia',
                    'Possess Drug Paraphernalia', 'Prostitution/Lewd Act Assignation', 'D.U.I. Serious Bodily Injury', 'Agg Fleeing/Eluding High Speed',
                    'Deliver Cannabis 1000FTSch', 'Oper Motorcycle W/O Valid DL', 'Use Scanning Device to Defraud', 'Deliver Cocaine 1000FT School',
                    'Possession of Oxycodone', 'Fish/Wildlife Viol Landing Requirements', 'Possession of Cocaine', 'Reckless Display Of Weapon',
                    'Credit Card Theft', 'False Verif Ownership Pawn Shp', 'Fleeing Or Attmp Eluding A Leo', 'Intoxicating Beverages', 'DUI - Enhanced',
                    'Fraud Obtain Driver License', 'Sale of Alcoholic Bev to Minor', 'Trafficking In Cocaine,+28-200', 'Harass Witness/Victm/Informnt',
                    'DWLS Child Support 1st Offense', 'Drivg While Lic Suspd/Revk/Can', 'No Wholesale/ Retail Dealer Lic', 'Refuse Submit Blood/Breath Test',
                    'Loiter Where Sign is Posted', 'Deliver Cannabis', 'Expired Tag/ Reg>6 Months 2nd', 'Aid/Abet Burglary Assault/Batt', 'Criminal Mischief',
                    'DWLS/License Susp/Revoked', 'Food License Violation', 'Trespass After Warning', 'Poss Unlaw Issue Id', 'Arson in the Second Degree',
                    'Posses/Disply Susp/Revk/Frd DL', 'Disorderly Intoxication', 'Carry Open/Uncov Bev In Pub', 'Retail Theft', 'DOC/Fighting/Threatening Words',
                    'S/M/D/P/W/Int 1000 Sch/Child C', 'Resisting W/O Violence', 'Poss Of RX Without RX', 'Poss Similitude of Drivers Lic',
                    'Use Computer for Child Exploit', 'Driving While License Revoked', 'Possess Cannabis/20 Grams Or Less', 'Possession Of Heroin',
                    'Fail to Report Change/Residence', 'Attempted Robbery  No Weapon', 'Possession False Prescription', 'Tresspass in Struct/Convey Occupy',
                    'Burglary Conveyance Occupied', 'Tampering With Physical Evidence', 'Wear Mask w/Commit Offense', 'Unlaw Use False Name/Identity',
                    'Resist/Obstruct W/O Violence', 'False Ownership Info/Pawn Item', 'Att Burgl Struc/Conv Dwel/Occp', 'Uttering a Forged Instrument',
                    'Extradition/Defendants', 'Poss Cocaine/Intent To Del/Sel', 'Opert With Susp DL 2nd Offens', 'Tampering With A Victim',
                    'Tamper With Witness/Victim/CI', 'Drink/Premises Licensed Estab', 'Operating W/O Valid License', 'Poss Of Controlled Substance',
                    'Petit Theft/ Prior Conviction', 'Grand Theft of the 2nd Degree', 'Tresspass Struct/Conveyance', 'Possession of Cannabis',
                    'Disorderly Conduct', 'Possession of XLR11', 'Poss Unlaw Issue Driver Licenc', 'Misuse Of 911 Or E911 System', 'Possession Of Alprazolam',
                    'Use Fraud OBT Rcpt/False Rcpt', 'Unlicensed Contractor', 'Leaving the Scene of Accident', 'Petit Theft $100- $300', 'Poss Mot Veh Mfg "ID" Removed',
                    'Drinking Alch Beverage In Open', 'Procure Person Und 18/Prostitu', 'Failure To Return Hired Vehicle', 'Felon in Pos of Firearm or Amm',
                    'Driving Under The Influence', 'Fail Register Career Offender', 'Fraudulent Use Of Credit Card', 'Cruelty To Animals', 'Viol Injunction Protect Dom Vi',
                    'Trespass', 'Retaliate Wit/Vict No Injury', 'Robbery / No Weapon', 'Retail Theft $300 1st Offense', 'Escape', 'Unlawful Assembly',
                    'Sex Offender Fail Comply W/Law', 'Use/Poss/Drug Para/Plant/Grow', 'Felony Committing Prostitution', 'Forge Revoked Expired Credit Card',
                    'Procure For Prostitution', 'DWLS Habitual Offender 2nd', 'Possession Burglary Tools', 'Traffick Oxycodone 14-25 grams',
                    'Traffick Oxycodone    28g><30k', 'Purchase Of Cocaine', 'Tresspass in Structure or Conveyance', 'Sleeping On Beach', 'Petit Theft',
                    'Poss/Sell/Del Cocaine 1000FT Sch', 'Poss Meth/Diox/Meth/Amp (MDMA)', 'Possession Of Cocaine 1000FT Sch', 'Crim Use of Personal ID Info',
                    'DUI Property Damage/Injury', 'Disobey Officer/Fireman', 'Lve/Scen/Acc/Veh/Prop/Damage', 'Fail Obey Driv Lic Restrictions',
                    'Possession Of Methamphetamine', 'S/M/D/P/W/I Sch 1a,1b,1d,2a,2b', 'Permit Unauthorized Minor Drv', 'Fail Change Address On Veh Reg',
                    'Deliver Cocaine', 'Unlaw Malic Strike K9/Horses', 'Driving License Suspended', 'Trafficking 4-14 Grams Heroin', 'Exposes Culpable Negligence',
                    'Traffick Amphetamine 14g><28g', 'Compulsory Attendance Violation', 'Uttering Forged Bills', 'Retail/Farm/Fare/Theft', 'Giving False Crime Report',
                    'Dealing in Stolen Property', 'Viol Injunct Domestic Violence', 'Grand Theft Firearm', 'Resist Merchant W Or W/O Viol',
                    'Trespass/Property/Other Structure', 'Attempted Burg/struct/unocc', 'DWLS Suspend Cancel Revoked', 'Defrauding Innkeeper',
                    'Misuse Of Wireless 911 System', 'Sell Cannabis 1000FTSch', 'Trafficking In Cocaine 28><200', 'Trespass Other Struct/Conve',
                    'Grand Theft (Motor Vehicle)', 'Poss of Cocaine W/I/D/S 1000FT Park', 'Defrauding Innkeeper $300/Less', 'Grand Theft Dwell Property',
                    'Violation License Restrictions', 'Inhale Harmful Substance', 'Present Proof of Invalid Insur', 'Burglary Unoccupied Dwelling',
                    'Use of Anti-Shoplifting Device', 'Unnatural/Lascivious Act', 'Fraudulent Use of Credit Card', 'Att Burgl Unoccupied Dwel', 'Trespass Structure/Conveyance',
                    'No/Improper Drivers License', 'Obtain Control Substance By Fraud', 'Obstruct Officer By Disguise', 'Trespassing', 'Petit Theft Habitual Offender',
                    'Viol Pretrial Release Dom Viol', 'Poss Handcuff Key While IC', 'Attempted Escape', 'Sel/Man/Del/Pos/W/Int Diazepam', 'Trespassing/Construction Site',
                    'Solicit To Deliver Cocaine', 'Fail to Reg as Sexual Offender', 'Create/Poss/Use Fictitious Personal ID Info',
                    'Felony/Driving Under Influence', 'Deposit Item W/I/Defraud', 'Live on Earnings of Prostitute', 'DUI Blood Alcohol Above 0.20',
                    'Deliver Cannabis 1000FTChur', 'Possession Of 3,4Methylenediox', 'Purchase Cannabis', 'Poss Oxycodone W/Int/Sell/Del',
                    'Poss Contr Subst W/o Prescript', 'Solicit To Deliver Cocaine', 'Possession Of Oxymorphone', 'Possession Of Amphetamine',
                    'Crim/Transmit of HIV Virus', 'Insurance Fraud', 'Poss 3,4 MDMA (Ecstasy)', 'Trafficking In Methamphetamine',
                    'Att Tamper w/Physical Evidence', 'Fel Drive License Perm Revoke', 'Agg Fleeing and Eluding', 'Defrauding Innkeeper $300/More',
                    'Poss Tetrahydrocannabinols', 'Poss Firearm W/Altered ID#', 'Sell Conterfeit Cont Substance', 'False Name By Person Arrest',
                    'Flee/Elude LEO-Agg Flee Unsafe', 'Fail To Redeliv Hire/Leas Prop', 'Failure To Pay Taxi Cab Charge', 'Agg Fleeing and Eluding',
                    'Lewdness Violation', 'Solicit Purchase Cocaine', 'Grand Theft in the 1st Degree', 'Unlaw Fail Secur Work Comp Ins',
                    'Leaving Acc/Unattended Veh', 'Voyeurism', 'Forging Bank Bills/Promis Note', 'Contempt/Fail To Appear/Summons',
                    'False Bomb Report', 'Leave Accd/Attend Veh/Less $50', 'Consp Traff Oxycodone 28g><30k', 'Unemployment Compensatn Fraud',
                    'Consp to Traffic Methamphetamine', 'Arson II (Vehicle)', 'Deliver Alprazolam', 'Manufacture Cannabis',
                    'Attempted Robbery Firearm', 'Fail To Secure Load', 'Misuse of Dealer Tag', 'Traff In Cocaine <400g>150 Kil',
                    'Deliver Cocaine 1000FT Church', 'Simulation of Legal Process', 'Grand Theft of a Fire Extinquisher',
                    'Fighting/Baiting Animals', 'Possession Of Fentanyl', 'Imperson Public Officer or Emplyee', 'Delivery of 5-Fluoro PB-22',
                    'Pos Cannabis For Consideration', 'Poss of Ethylone W/I/D/S', 'Grand Theft Unlawful Use CC', 'Sale/Del Counterfeit Cont Subs',
                    'Possession Child Pornography', 'Crim Attempt/Solicit/Consp', 'License Suspended Revoked', 'Fraud Obtain Food or Lodging',
                    'Money Launder 100K or More Dols', 'Intoxicated/Safety Of Another', 'Gambling/Gamb Paraphernalia', 'Del of JWH-250 2-Methox 1-Pentyl',
                    'Purchasing Of Alprazolam', 'Unauthorized Interf w/Railroad', 'Conspiracy Dealing Stolen Prop', 'Delivery of Heroin',
                    'DUI - Property Damage/Personal Injury', 'Exploit Elderly Person 20-100K', 'Possession Of Buprenorphine',
                    'Offer Agree Secure For Lewd Act', 'Prostitution/Lewdness/Assign', 'Neglect Child / Bodily Harm',
                    'Sound Articles Over 100', 'Ride Tri-Rail Without Paying', 'Disrupting School Function', 'Poss of Firearm/Ammun/Dom Viol',
                    'Poss Cntrft Contr Sub w/Intent', 'Counterfeit Lic Plates/Sticker', 'Sale/Del Cannabis At/Near Scho',
                    'Refuse to Supply DNA Sample', 'Alcohol Bev Under Age Of 21', 'Lewd Act Presence Child 16-',
                    'Traffick Amphetamine 28g><200g', 'Solic to Commit Battery', 'Poss of Methylethcathinone',
                    'Poss Alprazolam W/int Sell/Del', 'Possession of Benzylpiperazine', 'Poss Trifluoromethylphenylpipe',
                    'Possession of Butylone', 'Sel/Pur/Mfr/Del Control Substa', 'Poss Drugs W/O A Prescription', 'Soliciting For Prostitution',
                    'Possess Mot Veh W/Alt Vin #', 'Solicit Deliver Cocaine', 'DWI w/Inj Susp Lic / Habit Off', '	Sell or Offer for Sale Counterfeit Goods',
                    'Poss of Methylethcathinone', 'Fail Surrender Tag/Reg/DL', 'Obscenity', 'Use Of 2 Way Device To Fac Fel',
                    'DUI/Property Damage/Persnl Inj', 'Traveling to Meet a Minor', 'Hiring with Intent to Defraud',
                    'Video Voyeur-<24Y on Child >16', 'Exhibition Weapon School Prop', 'Violation Of Boater Safety Id',
                    'Possess Altered Bills', 'Del Cannabis At/Near Park', 'Retail Theft $300 2nd Offense',
                    'Accessory After the Fact', 'Possession of LSD', 'False 911 Call', 'Sell or Offer for Sale Counterfeit Goods',
                    'Crim Conspiracy 2nd Deg Felony', 'Sell Cocaine', 'Solicitation On Felony 3 Deg',
                    'Att Robbery Sudd Snatch No Weap', 'Crim Use Of Personal Id Info', 'Possession Of Diazepam',
                    'Fail To Redeliver Hire Prop', 'Unl/Disturb Education/Instui', 'Pos Gamma-Hydroxybutyric Acid',
                    'DWLS Susp/Cancel Revoked', 'Deliver Cocaine 1000FT Store', 'Solicitation On Felony 3 Deg',
                    'Carjacking w/o Deadly Weapon', 'Poss 5-Methoxy-N-methyl-N-isop', 'Contribute Delinquency Of A Minor',
                    'Theft of Trade Secrets', 'Possession of Codeine', 'Alcoholic Beverage Violation-FL',
                    'Compulsory Sch Attnd Violation', 'Driving While Intoxicated', 'Aiding Escape', 'Poss/pur/sell/deliver Cocaine',
                    'Offer Agree Secure/Lewd Act', 'Del Morphine at/near Park', 'Possess Tobacco Product Under 18', 'Grand Theft (motor Vehicle)',
                    'Trans/Harm/Material to a Minor', 'Delivery Of Trenbolon Acetat', 'Solicitation On Felony 3 Deg',
                    'Possession Of Phentermine', 'Interference with Custody', 'Traffic Counterfeit Cred Cards',
                    'Leaving Scene Accident w/Death', 'Obtain Property Worthless Check', 'Poss Pyrrolidinovalerophenone W/I/D/S',
                    'Pos Methylenedioxymethcath W/I/D/S', 'Offn Against Intellectual Prop', 'Poss Of 1,4-Butanediol',
                    'Poss/Sell/Deliver Clonazepam', 'Traffick Oxycodone     4g><14g', 'Interfere W/Traf Cont Dev RR',
                    'Attempted Burg/Convey/Unocc', 'Fabricating Physical Evidence', 'DOC/Cause Public Danger', 'Fail Sex Offend Report Bylaw',
                    'Contradict Statement', 'Agg Fleeing And Eluding', 'Unlaw Lic Use/Disply Of Others', 'Del 3,4 Methylenedioxymethcath',
                    'Discharge Firearm From Vehicle', 'DUI Property Damage', 'Opert With Susp DL 2ND Offense', 'Bribery Athletic Contests',
                    'Poss/Sell/Del/Man Amobarbital', 'Lease For Purpose Trafficking', 'Unlawful Alter Of Tag', 'Crim Attempt/Solic/Consp',
                    'Cruelty to Animals', 'Delivery Of Drug Paraphernalia', 'Trespass Private Property', 'Attempt/Solicitation/Conspiracy',
                    'Cause Anoth Phone Ring Repeat', 'PL/Unlaw Use Credit Card', 'Tamper With Witness', 'Temporary Tag', 'Possession Of Cocaine',
                    'Consp Traff Oxycodone  4g><14g', 'Consume Alcoholic Bev Pub', 'Pos Cannabis W/I/D/S Near Worsh',
                    'Forge Counterfeit Credit Card', 'Delivery Of Drug Paraphernalia', 'Theft', 'Possession Of MDMA W/I/D/S',
                    'Uttering Forged Credit Card', 'Impair/Impede Phone/Power Dwell', 'Structuring Transactions',
                    'Reduced/Amended to Reckless Driving', 'Principal In The First Degree', 'Impersonating Police Officer',
                    'Del Cannabis For Consideration', 'False Info LEO During Invest', 'Littering',
                    'Criminal Attempt 3rd Deg Felon', 'Possess Weapon On School Prop', 'Possession of Alcohol Under 21',
                    'Unlicensed Telemarketing', 'Lewd or Lascivious Exhibition', 'Aide/Abet Prostitution Lewdness', 'Possession Firearm School Prop',
                    'Poss of Vessel w/Altered ID NO', 'Poss Pyrrolidinobutiophenone', 'Burglary Damage Property>$1000', 'Organized Fraud',
                    'Aggravated Animal Cruelty', 'Unlawful Conveyance of Fuel', 'Issuing a Worthless Draft', 'Conspiracy to Deliver Cocaine',
                    'Sell Cannabis 1000FTChur', 'Falsely Impersonating Officer', 'Felony DUI - Enhanced', 'Computer Pornography',
                    'Aggress/Panhandle/Beg/Solict', 'Purchase/P/W/Int Cannabis', 'Uttering Worthless Check +$150', 'Dispense Optical Dev w/out Prescr',
                    'Lewd/Lasc Exhib Presence <16yr', 'Dealing In Stolen Property', 'Trespass On School Grounds', 'Workers Compensation Fraud',
                    'Poss 3,4 Methylendioxpyrovlerone', 'Sell Cannabis', 'Possess Controlled Substance', 'Unlawful Use Of Police Badges',
                    'Manage Busn W/O City Occup Lic', 'Discharge Firearm in Public/Res', 'Sel Etc/Pos/w/Int Contrft Schd',
                    'Possession Of Anabolic Steroid', 'Remove Cart/Retail Estab', 'Traffic Cannabis/25-2000lbs', 'Deliver Oxycodone',
                    'Burglary Structure Occupied', 'Attempted Deliv Control Subst', 'Att Burgl Occp Dwelling',
                    'DWLS Fin Obligation 1stOfense', 'Unauthor Copy of Sound Article', 'Defraud To Obtain Food/Lodg')

# Add column that represents whether the crime was violent 'V', non-violent 'N', or arrest case no charge/blank 'O'
data <- data %>% rowwise() %>%
  mutate(c_charge_violent = ifelse(c_charge_desc %in% violent, 'V',
                                   ifelse(c_charge_desc %in% non_violent, 'N', 'O')))

# Convert jail time stamps into datetime type
data$c_jail_in <- ymd_hms(data$c_jail_in)
data$c_jail_out <- ymd_hms(data$c_jail_out)

# Add column that represents how long person spent in jail in days
data <- data %>% rowwise() %>%
  mutate(c_time_in_jail = difftime(c_jail_out, c_jail_in, units = "days"))

# Set rows without a time for jail to 0
data$c_time_in_jail[is.na(data$c_time_in_jail)] <- 0

# Change time spent in jail's data type to be a number
data <- transform(data, c_time_in_jail = as.numeric(c_time_in_jail))

# Export data frame to csv
write.csv(data,'./data/compas-scores-updated.csv', row.names = FALSE)