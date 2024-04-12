% wuenic_ver_4.pl version 4.0
%
% Implements WHO & UNICEF rules for estimating national infant
% immunization coverage. Includes explanations and grade of confidence
% in estimate. Based on methods described in:
%
% Burton A, Monash R, Lautenbach B, Gacic-Dobo M, Neill M, Karimov R,
% Wolfson L, Jones G, Birmingham M. WHO and UNICEF estimates of national
% infant immunization coverage: methods and processes. Bull World Health
% Organ 2009; 87:535-541.
% https://www.who.int/bulletin/volumes/87/7/08-053819.pdf
%
% Burton A, Gacic-Dobo M, Karimov R, Kowalski R. A computational
% logic-based representation of the WHO and UNICEF estimates of national
% immunization coverage. DRAFT 23 January 2011. Articles and code
% available at: https://github.com/mgondan/wuenic
%
% Original code (V3) written by Tony Burton, System Analyst, Strategic
% Information Group, Expanded Programme on Immunization, Department of
% Immunization, Vaccines, and Biologicals. World Health Organization,
% 1211 Geneva 27, Switzerland
%
% Current version written by Matthias Gondan, Department of Psychology,
% University of Innsbruck, Austria, Matthias.Gondan-Rochon@uibk.ac.at
%
% This program works with the SWI-Prolog compiler,
% https://xsb.sourceforge.net/
%
% * Created: 6 November 2011
% * V3: 6 May 2016. ipv1 & rcv1 added, GoC for rcv1 based on mcv GoC
% * V3.9: 6 Dec 2023. Simplified the code for better maintainance
% * V4.0: 8 Dec 2023. Corrected some inconsistencies in the output.
%   Does not affect coverage estimates.
%
% WHO and UNICEF working group members as of 1 January 2010 - 30 April
% 2012:
% * Dr David BROWN, UNICEF/New York (dbrown@unicef.org)
% * Mr Tony BURTON, WHO/Geneva (burtona@who.int)
% * Ms Marta GACIC-DOBO, WHO/Geneva (gacicdobom@who.int)
% * Mr Rouslan KARIMOV, UNICEF/NEW YORK (rkarimov@unicef.org)
% * Dr Robert KOWALSKI, Imperial College London
%   (r.kowalski@imperial.ac.uk)

:- table anchor/6.
:- table survey/5.
:- table reported/5.
:- table wuenic_I/6.

% Separator is used in surveys
:- op(500, xfy, :).

% Ignore large jumps between Y - 1, Y, and Y + 1
sawtooth_threshold(10).

% Tolerate differences between survey and reported up to some threshold
survey_reported_threshold(10).

% Criteria for confidence in surveys
confidence_survey_scope(2).
confidence_survey_threshold(10).
confidence_UNPD_threshold(10).

% Recall bias is estimated by comparing the first and third dose of a
% vaccine
vaccine(dtp3, dtp1).
vaccine(pol3, pol1).
vaccine(hib3, hib1).
vaccine(hepb3, hepb1).
vaccine(pcv3, pcv1).

% Country-specific predicates describe the data, survey_results, working
% group decisions and when an estimate is required. Commented out to
% avoid warnings.
%
% admin(country, vaccine, year, coverage).
% gov(country, vaccine, year, coverage).
% vaccinated(country, vaccine, year, vaccinated).
% target(country, vaccine, year, vaccinated).
% legacy(country, vaccine, year, coverage).
% survey_results(country, vaccine, year, id, description, coverage).
% wgd(country, vaccine, year1, year2, action, explanation, covid1,
% covass1, covid2, covass2).
% births_UNPD(country, year, births).
% si_UNPD(country, year, surviving_infants).
%
% Some predicates are missing in some countries. The helper predicates
% below avoid error messages.
survey_results0(C, V, Y, ID, Description, Coverage) :-
    current_predicate(survey_results/6),
    survey_results(C, V, Y, ID, Description, Coverage).

vaccinated0(C, V, Y, Vaccinated) :-
    current_predicate(vaccinated/4),
    vaccinated(C, V, Y, Vaccinated).

% MG, discuss: Do we need support for unbounded integers?
% Example: target(irq,mcv2,2005,9744095698508)
target0(C, V, Y, Vaccinated) :-
    current_predicate(vaccinated/4),
    target(C, V, Y, Vac),
    Vac < 2^31,
    !,
    Vaccinated = Vac.

target0(C, V, Y, Vaccinated) :-
    current_predicate(vaccinated/4),
    target(C, V, Y, _Vac),
    !,
    Vaccinated = ''.

admin0(C, V, Y, Coverage) :-
    current_predicate(admin/4),
    admin(C, V, Y, Coverage).

% As a default, the first rubella dose is given with the second measles
% dose (see country-specific files for other options)
firstRubellaAtSecondMCV(_C, rcv1, _Y, mcv2).

% Year range for estimation. This needs to be changed each year.
year_range(1997, 2022).

% Save all estimates in (country).pl.v40.txt
estimate :-
    country(Code, Country),
    date(Date),
    setof([ Country, Date, Code, Vaccine, Year, Coverage, PrevRev, GC,
            Admin, Gov, Reported, Vaccinated, Target, UnpdBirths,
            UnpdSI, SeriesValue, Source, SurveyInfo, Rule, Explanation
          ],
    wuenic(Code, Vaccine, Year, Rule, Explanation, Coverage,
            PrevRev, GC, Admin, Gov, Reported, Vaccinated, Target,
            UnpdBirths, UnpdSI, Source, SeriesValue, SurveyInfo),
        Estimates),
    concat_atom(['out/', Code, '.txt'], File),
    Header = [ 'Country', 'ProductionDate', 'ISOCountryCode', 'Vaccine',
        'Year', 'WUENIC', 'WUENICPreviousRevision', 'GradeOfConfidence',
        'AdministrativeCoverage', 'GovernmentEstimate',
        'ReportedCoverage', 'ChildrenVaccinated', 'ChildrenInTarget',
        'BirthsUNPD', 'SurvivingInfantsUNPD', 'ReportedTimeSeries',
        'ReportedTimeSeriesSource', 'SurveyInformation', 'Rule',
        'Comment' ],
    open(File, write, Out, [encoding(utf8)]),
    output_fields(Header, Out),
    output_results(Estimates, Out),
    close(Out).

% Collects estimates, grade of confidence, and all explanations
wuenic(C, V, Y, Rule, Expl, Coverage, Prev, GC, Admin, Gov, Reported,
        Vaccinated, Target, Births, SI, Source, Series, Survey) :-
    estimate_required(C, V, Y, _, _),
    wuenic_I(C, V, Y, Rule, Expl0, Cov0),
    bound_0_100(Cov0, Coverage),
    confidence(C, V, Y, GoCExpl, GC),
    collect_data(C, V, Y, Prev, Admin, Gov, Reported, Vaccinated,
        Target, Births, SI, Series, Source, Survey),
    change_from_previous(C, V, Y, Coverage, Change),
    collect_explanations(C, V, Y, Text),
    append([Expl0, ' '], Text, Expl1),
    append(Expl1, [' ', Change, ' ', GoCExpl], Expl2),
    concat_atom(Expl2, Expl).

% Confidence depends on converging evidence from the different sources.
% 1 star = low confidence, ..., 3 stars = high confidence
%
% Copy rcv1 from mcv1
confidence(C, rcv1, Y, Expl, Grade) :-
    estimate_required(C, rcv1, Y, _, na),
    !,
    confidence(C, mcv1, Y, Expl, Grade).

% Copy rcv1 from mcv2
confidence(C, rcv1, Y, Expl, Grade) :-
    estimate_required(C, rcv1, Y, _, mcv2),
    !,
    confidence(C, mcv2, Y, Expl, Grade).

% Confidence rated by working group
confidence(C, V, Y, Expl, Grade) :-
    decision(C, V, Y, assignGoC, Expl0, _, Grade0),
    !,
    concat_atom(['GoC=Assigned by working group. ', Expl0], Expl),
    Grade = Grade0.

% Confidence in both reported, surveys, and sold vaccines
confidence(C, V, Y, Expl, Grade) :-
    conf_reported(C, V, Y, 'R+'),
    conf_survey(C, V, Y, 'S+'),
    conf_denominator(C, V, Y, 'D+'),
    !,
    Expl = 'GoC=R+ S+ D+',
    Grade = 3.

% If any estimate has been challenged, confidence is low
confidence(C, V, Y, Expl, Grade) :-
    setof(Expl0, challenge(C, V, Y, Expl0), List),
    !,
    concat_atom(['Estimate challenged by: ' | List], Expl),
    Grade = 1.

% Confidence in one or two sources, two stars
confidence(C, V, Y, Expl, Grade) :-
    conf_reported(C, V, Y, 'R+'),
    conf_survey(C, V, Y, 'S+'),
    !,
    Expl = 'GoC=R+ S+',
    Grade = 2.

confidence(C, V, Y, Expl, Grade) :-
    conf_survey(C, V, Y, 'S+'),
    conf_denominator(C, V, Y, 'D+'),
    !,
    Expl = 'GoC=S+ D+',
    Grade = 2.

confidence(C, V, Y, Expl, Grade) :-
    conf_reported(C, V, Y, 'R+'),
    conf_denominator(C, V, Y, 'D+'),
    !,
    Expl = 'GoC=R+ D+',
    Grade = 2.

confidence(C, V, Y, Expl, Grade) :-
    conf_reported(C, V, Y, 'R+'),
    !,
    Expl = 'GoC=R+',
    Grade = 2.

confidence(C, V, Y, Expl, Grade) :-
    conf_survey(C, V, Y, 'S+'),
    !,
    Expl = 'GoC=S+',
    Grade = 2.

confidence(C, V, Y, Expl, Grade) :-
    conf_denominator(C, V, Y, 'D+'),
    !,
    Expl = 'GoC=D+',
    Grade = 2.

% Low confidence
confidence(_C, _V, _Y, Expl, Grade) :-
    !,
    Expl = 'GoC=No accepted empirical data',
    Grade = 1.

% Check if any source is challenged
challenge(C, V, Y, 'R-') :-
    conf_reported(C, V, Y, 'R-').

challenge(C, V, Y, 'S-') :-
    conf_survey(C, V, Y, 'S-').

challenge(C, V, Y, 'D-') :-
    conf_denominator(C, V, Y, 'D-').

% Confidence in reported coverage if it is an anchor point. Otherwise,
% no confidence
conf_reported(C, V, Y, Support) :-
    reported(C, V, Y, _, _),
    wuenic_I(C, V, Y, Rule, _, _),
    !,
    (   member(Rule, ['R:', 'R: AP'])
    ->  Support = 'R+'
    ;   Support = 'R-'
    ).

% No confidence in surveys if _any_ survey deviates too much from WUENIC
% coverage
%
% MG, discuss: arm/pcv3 has a survey in 2013, but it is not taken into
% account because estimate_required starts only in 2015. Do we actually
% want this?
conf_survey(C, V, Y, Support) :-
    wuenic_I(C, V, Y, _, _, Cov0),
    estimate_required(C, V, Year, _, _),
    survey(C, V, Year, _, Coverage),
    confidence_survey_scope(Scope),
    abs(Y - Year) =< Scope,
    confidence_survey_threshold(Threshold),
    abs(Cov0 - Coverage) > Threshold,
    !,
    Support = 'S-'.

% Confidence only if all surveys are consistent with WUENIC coverage
conf_survey(C, V, Y, Support) :-
    wuenic_I(C, V, Y, _, _, _Cov0),
    estimate_required(C, V, Year, _, _),
    survey(C, V, Year, _, _Coverage),
    confidence_survey_scope(Scope),
    abs(Y - Year) =< Scope,
    !,
    Support = 'S+'.

% Todo list from V3
%
% 1. Simplify previous rule to a check for an anchor point
%
% supporting_survey_in_scope(C, V, Y, Rule) :-
%     survey(C, V, Y, _, _),
%     wuenic_I(C, V, Y, 'S: AP', _, _).
%
% 2. Rewrite rule to look at relationship between estimate rule and
% surveys in scope rule. For example, take randomness into account,
% e.g. probability for inconsistent results increases with the number of
% surveys and decreases with the sample size of the surveys.

% Recalculate coverage using reported number of children vaccinated and
% births and surviving infants from UNPD estimates.
conf_denominator(C, V, Y, Support) :-
    vaccinated0(C, V, Y, _),
    births_UNPD(C, Y, _),
    si_UNPD(C, Y, _),
    wuenic_I(C, V, Y, _Rule, _Expl, Cov0),
    denominator(C, V, Y, Coverage),
    !,
    confidence_UNPD_threshold(Threshold),
    (   abs(Coverage - Cov0) < Threshold % MG: < inconsistent with conf_survey
    ->  Support = 'D+'
    ;   Support = 'D-'
    ).

% Births used for bcg and hepb birth dose
denominator(C, V, Y, Coverage) :-
    member(V, [bcg, hepbb]),
    !,
    vaccinated0(C, V, Y, Vaccinated),
    births_UNPD(C, Y, Births),
    Coverage is Vaccinated / Births * 100.

% Surviving infants for remaining vaccines
denominator(C, V, Y, Coverage) :-
    vaccinated0(C, V, Y, Vaccinated),
    si_UNPD(C, Y, SI),
    Coverage is Vaccinated / SI * 100.

% Flag modifications in the program code that change the coverage
% estimates
change_from_previous(C, V, Y, Coverage, Change) :-
    legacy(C, V, Y, Legacy),
    Legacy \= Coverage,
    !,
    concat_atom(['Estimate of ', Coverage,
        ' percent changed from previous revision value of ',
        Legacy,' percent. '], Change).

change_from_previous(_C, _V, _Y, _, '').

% Obtain estimates for vaccine coverage. At Level 1, check
% for working group decisions and work around obvious inconsistencies.
%
% Assigned by working group
wuenic_I(C, V, Y, Rule, Expl, Coverage) :-
    decision(C, V, Y, assignWUENIC, Expl0, _, Cov0),
    !,
    Rule = 'W:',
    Expl = Expl0,
    Coverage = Cov0.

% If DTP3 > DTP1 (which is impossible), estimate coverage using equation
wuenic_I(C, dtp1, Y, Rule, Expl, Coverage) :-
    wuenic_II(C, dtp1, Y, _Rule, _Expl, DTP1),
    wuenic_II(C, dtp3, Y, _, _, DTP3),
    DTP3 > DTP1,
    !,
    Rule = 'RMF:',
    concat_atom(['Estimate based on DTP3 coverage of ',
        DTP3, '. '], Expl),
    Coverage is round(DTP3 - 0.0058 * DTP3 * DTP3 + 0.3912 * DTP3 + 18.258).

% Estimate for DTP1 plausible
wuenic_I(C, dtp1, Y, Rule, Expl, Coverage) :-
    wuenic_II(C, dtp1, Y, Rule0, Expl0, Cov0),
    !,
    Rule = Rule0,
    Expl = Expl0,
    Coverage = Cov0.

% If DTP1 not reported: estimate using equation
wuenic_I(C, dtp1, Y, Rule, Expl, Coverage) :-
    wuenic_II(C, dtp3, Y, _, _, DTP3),
    !,
    Rule = 'RMF:', % added colon for consistency
    concat_atom(['Estimate based on DTP3 coverage of ', DTP3, '. '], Expl),
    Coverage is round(DTP3 - 0.0058 * DTP3 * DTP3 + 0.3912 * DTP3 + 18.258).

% Estimate for RCV1 where RCV1 given at MCV2
wuenic_I(C, rcv1, Y, Rule, Expl, Coverage) :-
    estimate_required(C, rcv1, Y, _, FirstRubellaDose),
    firstRubellaAtSecondMCV(C, rcv1, Y, FirstRubellaDose),
    !,
    wuenic_II(C, mcv2, Y, Rule, _, Coverage),
    Expl = 'First dose of rubella vaccine given with second dose of measles containing vaccine. Estimate based on MCV2 estimate'.

% Estimate for RCV1 where RCV1 given at MCV1
wuenic_I(C, rcv1, Y, Rule, Expl, Coverage) :-
    estimate_required(C, rcv1, Y, _, _),
    !,
    wuenic_II(C, mcv1, Y, Rule, _, Coverage),
    Expl = 'Estimate based on estimated MCV1. '.

% Estimate for non-DTP1 & RCV1 vaccines.
wuenic_I(C, V, Y, Rule, Expl, Coverage) :-
    !,
    wuenic_II(C, V, Y, Rule, Expl, Coverage).

% Level II: Estimate coverage by distinguishing different cases
%
% * Estimate at anchor point
% * Estimate between anchor points
% * Estimate before anchor point
% * Estimate after anchor point
% * No anchor points
%
% At anchor points
wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
    anchor(C, V, Y, Rule0, Expl0, Cov0),
    !,
    Rule = Rule0,
    Expl = Expl0,
    Coverage = Cov0.

% Between anchor points: interpolation forced by working group
%
% MG, discuss: Move to Level I, because working group decisions should
% rule.
% MG, discuss: Prec and Succ Year relative to current year, should be
% relative to the decision. Example: bdi/bcg, 2000
wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
    decision(C, V, Y, interpolate, Expl0, _, _),
    prec_anchor(C, V, Y, Prec, _, PrecCov),
    succ_anchor(C, V, Y, Succ, _, SuccCov),
    !,
    Rule = 'W-I:',
    concat_atom(['Estimate informed by interpolation between ', Prec,
        ' and ', Succ, ' levels. ', Expl0], Expl),
    interpolate(Prec, PrecCov, Succ, SuccCov, Y, Coverage).

% Between anchor points: between two reported anchors
wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, Source, Cov0),
    prec_anchor(C, V, Y, _Prec, PrecRule, _),
    PrecRule = 'R: AP',
    succ_anchor(C, V, Y, _Succ, SuccRule, _),
    SuccRule = 'R: AP',
    !,
    Rule = 'R:',
    member(Source-Expl,
      [ gov-'Estimate informed by reported data. ',
        admin-'Estimate informed by reported administrative data. ',
        interpolated-'Estimate informed by interpolation between reported data. '
      ]),
    Coverage = Cov0.

% Between other anchor points (not both of type "reported"): calibrate
wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, _Source, Reported),
    prec_anchor(C, V, Y, Prec, PrecRule, PrecCov),
    succ_anchor(C, V, Y, Succ, SuccRule, SuccCov),
    ( PrecRule \= 'R: AP' ; SuccRule \= 'R: AP' ),
    !,
    Rule = 'C:',
    concat_atom(['Reported data calibrated to ', Prec,
        ' and ', Succ, ' levels. '], Expl),
    reported_time_series(C, V, Prec, _, PrecRep),
    reported_time_series(C, V, Succ, _, SuccRep),
    interpolate(Prec, PrecRep, Succ, SuccRep, Y, RepInterp),
    interpolate(Prec, PrecCov, Succ, SuccCov, Y, AnchInterp),
    Adj is AnchInterp - RepInterp,
    Coverage is round(Reported + Adj).

% Before earliest/after latest anchor (of type reported)
wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, Source, Cov0),
    (   succ_anchor(C, V, Y, _Anchor, AnchorRule, _AnchorCov),
        AnchorRule = 'R: AP'
    ;   prec_anchor(C, V, Y, _Anchor, AnchorRule, _AnchorCov),
        AnchorRule = 'R: AP'
    ), !,
    Rule = 'R:',
    member(Source-Expl,
      [ gov-'Estimate informed by reported data. ',
        admin-'Estimate informed by reported administrative data. ',
        interpolated-'Estimate informed by interpolation between reported data. ',
        extrapolated-'Estimate based on extrapolation from data reported by national government. '
      ]),
    Coverage = Cov0.

% Before earliest/after latest anchor (not of type reported): calibrated
wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, _, Cov0),
    (   succ_anchor(C, V, Y, Anchor, AnchorRule, AnchorCov),
        AnchorRule \= 'R: AP'
    ;   prec_anchor(C, V, Y, Anchor, AnchorRule, AnchorCov),
        AnchorRule \= 'R: AP'
    ), !,
    Rule = 'C:',
    concat_atom(['Reported data calibrated to ', Anchor, ' levels. '], Expl),
    reported_time_series(C, V, Anchor, _, ReportedAtAnchor),
    Adj is AnchorCov - ReportedAtAnchor,
    Coverage is round(Cov0 + Adj).

% No anchor points for any year, use reported
wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, Source, Cov0),
    !,
    Rule = 'R:',
    member(Source-Expl,
      [ gov-'Estimate informed by reported data. ',
        admin-'Estimate informed by reported administrative data. ',
        interpolated-'Estimate informed by interpolation between reported data. ',
        extrapolated-'Estimate informed by extrapolation from reported data. '
      ]),
    Coverage = Cov0.

% Determine coverage value at anchor points defined as years where there
% are multiple data points (reported | survey | wgd).
%
% Reported value "anchored" by working group
anchor(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, _, Cov0),
    decision(C, V, Y, assignAnchor, Expl0, _, Cov0), % same Cov0
    !,
    Rule = 'R: AP',
    Expl = Expl0,
    Coverage = Cov0.

% Working group assigns anchor point value.
anchor(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, _, _Cov0),
    decision(C, V, Y, assignAnchor, Expl0, _, Assigned),
    % Cov0 \= Assigned,
    !,
    Rule = 'W: AP',
    concat_atom(['Estimate of ', Assigned,
        ' percent assigned by working group. ', Expl0], Expl),
    Coverage = Assigned.

% Survey results support reported
%
% MG, discuss: Comparison with threshold is based on rounded
% numbers, which gives an incorrect result at
% anchor(bgd, dtp1, 2015, Info, Src, Cov)
anchor(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, Source, Cov0),
    survey(C, V, Y, Expl0, Survey),
    survey_reported_threshold(Threshold),
    abs(Cov0 - Survey) =< Threshold,
    !,
    Rule = 'R: AP',
    member(Source-Expl1,
      [ gov-'Estimate informed by reported data supported by survey. ',
        admin-'Estimate informed by reported administrative data supported by survey. ',
        interpolated-'Estimate informed by interpolation between reported data supported by survey. ',
        extrapolated-'Estimate based on extrapolation from data reported by national government supported by survey. '
      ]),
    concat_atom([Expl1, Expl0], Expl),
    Coverage = Cov0.

% Survey results challenge reported
anchor(C, V, Y, Rule, Expl, Coverage) :-
    reported_time_series(C, V, Y, _, _Cov0),
    survey(C, V, Y, Expl0, Survey),
    % survey_reported_threshold(Threshold),
    % abs(Cov0 - Survey) > Threshold,
    !,
    Rule = 'S: AP',
    concat_atom(['Survey evidence does not support reported data. Estimate based on survey results. ',
        Expl0, ' '], Expl),
    Coverage = Survey.

% Survey information for given year. Multiple surveys are averaged.
survey(C, V, Y, Expl, Coverage) :-
    findall(Cov, survey_accepted(C, V, Y, _, Cov), [H | T]),
    length([H | T], N),
    sum_list([H | T], Sum),
    Coverage is round(Sum / N),
    concat_atom(['Survey evidence of ', Coverage, ' percent based on ',
        N, ' survey(s). '], Expl).

% Reasons to exclude a survey include:
%    Sample size < 300,
%    The working group decides to exclude the survey.
survey_accepted(C, V, Y, ID, Coverage) :-
    survey_for_analysis(C, V, Y, ID, Desc, Cov0),
    (   decision(C, V, Y, acceptSurvey, _, ID, _)
    ;   member(ss:Size, Desc),
        Size >= 300,
        not(decision(C, V, Y, ignoreSurvey, _, ID, _)),
        not(decision(C, V, Y, ignoreSurvey, _, na, _))
    ),
    % Check if survey needs to be modified
    (   survey_modified(C, V, Y, ID, _, Modified)
    ->  Coverage = Modified
    ;   Coverage = Cov0
    ).

% Survey results modified for recall bias
%
% MG, V4: compare to rounded survey coverage (e.g., bfa dtp3 in 2019)
survey_modified(C, V, Y, ID, Expl, Coverage) :-
    member(V, [dtp3, pol3, hib3, hepb3, pcv3]),

    % Third dose, card only
    survey_results0(C, V, Y, ID, DescriptionCard3Dose, C3Cov),
    member(confirm:card, DescriptionCard3Dose),
    member(age:AgeCohortCard3Dose, DescriptionCard3Dose),
    member(AgeCohortCard3Dose, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),

    % First dose, card or history
    vaccine(V, First),
    survey_results0(C, First, Y, ID, DescriptionCoH1Dose, CoH1Cov),
    member(confirm:'card or history', DescriptionCoH1Dose),
    member(age:AgeCohortCoH1, DescriptionCoH1Dose),
    member(AgeCohortCoH1, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),

    % First dose, card only
    survey_results0(C, First, Y, ID, DescriptionCard1Dose, C1Cov),
    C1Cov > 0,
    member(confirm:card, DescriptionCard1Dose),
    member(age:AgeCohortCard1Dose, DescriptionCard1Dose),
    member(AgeCohortCard1Dose, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),

    Adj is C3Cov / C1Cov,
    ThirdHistoryAdj is (CoH1Cov - C1Cov) * Adj,
    CovAdjusted is C3Cov + ThirdHistoryAdj,
    bound_0_100(CovAdjusted, Cov0),

    survey_for_analysis(C, V, Y, ID, Description, SurveyCoverage),
    Cov0 =\= round(SurveyCoverage), % added rounding

    SurveyCovRounded is round(SurveyCoverage),
    CH1 is round(CoH1Cov),
    C1 is round(C1Cov),
    C3 is round(C3Cov),
    member(title:Title, Description),
    concat_atom(
      [ Title, ' card or history results of ', SurveyCovRounded,
        ' percent modifed for recall bias to ', Cov0,
        ' percent based on 1st dose card or history coverage of ', CH1,
        ' percent, 1st dose card only coverage of ', C1,
        ' percent and 3rd dose card only coverage of ', C3,
        ' percent. '
      ], Expl),
    Coverage = Cov0.

% Survey results passed for inclusion in the analysis include:
% card or history results for cohorts 12-23, 18-29, 15-26, 24-35 months
% of age
survey_for_analysis(C, V, Y, ID, Description, Coverage) :-
    survey_results0(C, V, Y, ID, Description, Coverage),
    member(confirm:'card or history', Description),
    member(age:AgeCohort, Description),
    member(AgeCohort, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']).

% Time series of reported data
%
% Reported data available
reported_time_series(C, V, Y, Source, Coverage) :-
    estimate_required(C, V, Y, _, _),
    reported(C, V, Y, Source0, Cov0),
    not(reported_rejected(C, V, Y)),
    !,
    Source = Source0,
    Coverage = Cov0.

% Interpolation, no data/reported data excluded between two years
reported_time_series(C, V, Y, Source, Coverage) :-
    estimate_required(C, V, Y, _, _),
    (   not(reported(C, V, Y, _, _))
    ;   reported_rejected(C, V, Y)
    ),
    year_before_reported(C, V, Y, Prec, PrecCov),
    year_after_reported(C, V, Y, Succ, SuccCov),
    !,
    Source = interpolated,
    interpolate(Prec, PrecCov, Succ, SuccCov, Y, Coverage).

% Extrapolation, latest required estimate
reported_time_series(C, V, Y, Source, Coverage) :-
    estimate_required(C, V, Y, _, _),
    (   not(reported(C, V, Y, _, _))
    ;   reported_rejected(C, V, Y)
    ),
    nearest_reported(C, V, Y, _Year, Cov0),
    !,
    Source = extrapolated,
    Coverage = Cov0.

% Reasons to exclude reported data are:
% * Working group decision.
% * Coverage > 100%
% * Inconsistent temporal changes (sawtooth or sudden change)

% Rejection dominates accepted, see below
reported_rejected(C, V, Y) :-
    decision(C, V, Y, ignoreReported, _Expl0, _, _),
    !.

% See above
reported_rejected(C, V, Y) :-
    decision(C, V, Y, acceptReported, _, _, _),
    !,
    fail.

% Implausible coverage
reported_rejected(C, V, Y) :-
    reported(C, V, Y, _, Coverage),
    Coverage > 100,
    !.

% Sudden jumps
reported_rejected(C, V, Y) :-
    reported(C, V, Y, _, Coverage),
    Prec is Y - 1,
    Succ is Y + 1,
    reported(C, V, Prec, _, PrecCov),
    reported(C, V, Succ, _, SuccCov),
    sawtooth_threshold(Threshold),
    (   Coverage - PrecCov > Threshold,
        Coverage - SuccCov > Threshold
    ;   PrecCov - Coverage > Threshold,
        SuccCov - Coverage > Threshold
    ), !.

% Sudden change in most recently reported data for classic vaccines, or
% sudden decline in most recently reported data for new vaccines
reported_rejected(C, V, Y) :-
    reported(C, V, Y, _, Coverage),
    not(reported_later(C, V, Y)),
    Prec is Y - 1,
    reported(C, V, Prec, _, PrecCov),
    sawtooth_threshold(Threshold),
    (   member(V, [pcv3, rotac])
    ->  PrecCov - Coverage > Threshold
    ;   abs(PrecCov - Coverage) > Threshold
    ), !.

reported_later(C, V, Y) :-
    year_range(_, Max),
    between(Y, Max, After),
    After > Y,
    reported(C, V, After, _, _).

% Reported to WHO and UNICEF is government estimate. If government
% estimate missing, then reported is administrative data. If both
% missing, fail.
reported(C, V, Y, Source, Coverage) :-
    gov(C, V, Y, Cov0),
    not(decision(C, V, Y, ignoreGov, _, _, _)),
    !,
    Source = gov,
    Coverage = Cov0.

reported(C, V, Y, Source, Coverage) :-
    admin0(C, V, Y, Cov0),
    (   decision(C, V, Y, ignoreGov, _, _, _)
    ;   not(gov(C, V, Y, _))
    ),
    not(decision(C, V, Y, ignoreAdmin, _, _, _)),
    !,
    Source = admin,
    Coverage = Cov0.

% Determine whether a working group decision applies for a given year
decision(C, V, Y, Action, Explanation, Survey, Coverage) :-
    wgd(C, V, Begin, End, Action, Explanation, Survey, Coverage, _, _),
    Begin =< Y, End >= Y.

% Extrapolation using nearest neighbor
nearest_reported(C, V, Y, Nearest, Coverage) :-
    year_before_reported(C, V, Y, Before, CoverageBefore),
    year_after_reported(C, V, Y, After, CoverageAfter),
    !,
    (   Y - Before < After - Y
    ->  Nearest = Before,
        Coverage = CoverageBefore
    ;   Nearest = After,
        Coverage = CoverageAfter
    ).

nearest_reported(C, V, Y, Nearest, Coverage) :-
    year_before_reported(C, V, Y, Before, CoverageBefore),
    !,
    Nearest = Before,
    Coverage = CoverageBefore.

nearest_reported(C, V, Y, Nearest, Coverage) :-
    year_after_reported(C, V, Y, After, CoverageAfter),
    !,
    Nearest = After,
    Coverage = CoverageAfter.

% Routines for interpolated points: search data in the past
year_before_reported(C, V, Y, Before, Coverage) :-
    year_range(Min, _),
    Dist is Y - Min,
    between(1, Dist, Minus),
    Bef0 is Y - Minus,
    reported(C, V, Bef0, _, Cov0),
    not(reported_rejected(C, V, Bef0)),
    !,
    Before = Bef0,
    Coverage = Cov0.

% Search in the future
year_after_reported(C, V, Y, After, Coverage) :-
    year_range(_, Max),
    between(Y, Max, Aft0),
    Aft0 > Y,
    reported(C, V, Aft0, _, Cov0),
    not(reported_rejected(C, V, Aft0)),
    !,
    After = Aft0,
    Coverage = Cov0.

% Get values of nearest surrounding anchor points
prec_anchor(C, V, Y, Prec, Rule, Coverage) :-
    year_range(Min, _),
    Dist is Y - Min, % MG: minimum from estimate_required
    between(1, Dist, Minus),
    Prec0 is Y - Minus,
    anchor(C, V, Prec0, Rule0, _Expl0, Cov0),
    !,
    Prec = Prec0,
    Rule = Rule0,
    Coverage = Cov0.

succ_anchor(C, V, Y, Succ, Rule, Coverage) :-
    year_range(_, Max),
    between(Y, Max, Succ0),
    Succ0 > Y,
    anchor(C, V, Succ0, Rule0, _Expl0, Cov0),
    !,
    Succ = Succ0,
    Rule = Rule0,
    Coverage = Cov0.

% Interpolate between two years
interpolate(Prec, PrecCov, Succ, SuccCov, Year, Coverage) :-
    Slope is (SuccCov - PrecCov) / (Succ - Prec),
    Coverage is round(PrecCov + (Year - Prec) * Slope).

% Ensure estimates are between 0 and 99
%
% MG, discuss: round only at output
bound_0_100(X, Y) :-
    Y is max(0, min(99, round(X))).

% Add underlying data to each C, V, Y estimate
collect_data(C, V, Y, Prev, Admin, Gov, Reported, Vaccinated,
        Target, Births, SI, Series, Source, Survey) :-
    legacy_estimate(C, V, Y, Prev),
    admin_data(C, V, Y, Admin),
    gov_data(C, V, Y, Gov),
    reported_data(C, V, Y, Reported),
    vaccinated_data(C, V, Y, Vaccinated),
    target_data(C, V, Y, Target),
    time_series_data(C, V, Y, Source, Series),
    unpd_births_data(C, Y, Births),
    unpd_si_data(C, Y, SI),
    survey_data(C, V, Y, Survey).

legacy_estimate(C, V, Y, Data) :-
    legacy(C, V, Y, Data0),
    !,
    Data = Data0.

legacy_estimate(_, _, _, '').

admin_data(C, V, Y, Data) :-
    admin0(C, V, Y, Data0),
    !,
    Data = Data0.

admin_data(_, _, _, '').

gov_data(C, V, Y, Data) :-
    gov(C, V, Y, Data0),
    !,
    Data = Data0.

gov_data(_, _, _, '').

reported_data(C, V, Y, Data) :-
    reported(C, V, Y, _, Data0),
    !,
    Data = Data0.

reported_data(_, _, _, '').

vaccinated_data(C, V, Y, Data) :-
    vaccinated0(C, V, Y, Data0),
    !,
    Data = Data0.

vaccinated_data(_, _, _, '').

target_data(C, V, Y, Data) :-
    target0(C, V, Y, Data0),
    !,
    Data = Data0.

target_data(_, _, _, '').

unpd_births_data(C, Y, Data) :-
    births_UNPD(C, Y, Data0),
    !,
    Data = Data0.

unpd_births_data(_, _, '').

unpd_si_data(C, Y, Data) :-
    si_UNPD(C, Y, Data0),
    !,
    Data = Data0.

unpd_si_data(_, _, '').

time_series_data(C, V, Y, Source, Value) :-
    reported_time_series(C, V, Y, Source0, Value0),
    !,
    Source = Source0,
    Value = Value0.

time_series_data(_, _, _, '', '').

survey_data(C, V, Y, Data) :-
    survey(C, V, Y, _, Data0),
    !,
    Data = Data0.

survey_data(_, _, _, '').

% Collect explanations in natural language terms
collect_explanations(C, V, Y, Explanations) :-
    findall(Expl, explanation(C, V, Y, Expl), Explanations).

explanation(C, V, Y, Expl) :-
    survey_reason_to_exclude(C, V, Y, Expl).

% Change in V4: report adjustment only for those surveys that have not
% been ignored by the working group (change has been reverted)
explanation(C, V, Y, Expl) :-
%   survey_accepted(C, V, Y, ID, _), % commented out again
    survey_modified(C, V, Y, _ID, Expl, _).

explanation(C, V, Y, Expl) :-
    reported_reason_to_exclude(C, V, Y, Expl).

explanation(C, V, Y, Expl) :-
    decision(C, V, Y, comment, Expl, _, _).

explanation(C, V, Y, Expl) :-
    decision(C, V, Y, acceptSurvey, Expl, _, _).

explanation(C, V, Y, Expl) :-
    decision(C, V, Y, acceptReported, Expl, _, _).

explanation(C, V, Y, Expl) :-
    decision(C, V, Y, ignoreGov, Expl, _, _).

% Reasons to exclude a survey include:
%    Sample size < 300,
%    The working group decides to exclude the survey.
%
% used for explanation/4
survey_reason_to_exclude(C, V, Y, Expl) :-
    survey_for_analysis(C, V, Y, ID, Description, _),
    not(decision(C, V, Y, acceptSurvey, _Expl, ID, _)),
    member(ss:Size, Description),
    Size < 300,
    concat_atom(['Survey results ignored. Sample size ', Size, ' less than 300. '], Expl).

% V4: Keeps explanations for the same survey together
%
% MG, discuss: only display if sample size > 300 to avoid long comments
survey_reason_to_exclude(C, V, Y, Expl) :-
    survey_for_analysis(C, V, Y, _ID, Description, _),
%    member(ss:Size, Description),
%    Size >= 300,
    decision(C, V, Y, ignoreSurvey, Expl0, na, _),
    member(title:Title, Description),
    concat_atom([Title, ' results ignored by working group. ', Expl0], Expl).

survey_reason_to_exclude(C, V, Y, Expl) :-
    survey_for_analysis(C, V, Y, ID, Description, _),
%    member(ss:Size, Description),
%    Size >= 300,
    decision(C, V, Y, ignoreSurvey, Expl0, ID, _),
    member(title:Title, Description),
    concat_atom([Title, ' results ignored by working group. ', Expl0], Expl).

% Reasons to exclude reported data are:
%  1. Working group decision.
%  2. Coverage > 100%
%  3. Inconsistent temporal changes (sawtooth or sudden change most
%  recent year)
%
%  semidet, needed for explanation/4
reported_reason_to_exclude(C, V, Y, Expl) :-
    reported(C, V, Y, _, _),
    decision(C, V, Y, ignoreReported, Expl0, _, _),
    concat_atom(['Reported data excluded. ', Expl0], Expl).

reported_reason_to_exclude(C, V, Y, Expl) :-
    reported(C, V, Y, _, Coverage),
    not(decision(C, V, Y, acceptReported, _, _, _)),
    Coverage > 100,
    concat_atom(['Reported data excluded because ', Coverage,
        ' percent greater than 100 percent. '], Expl).

reported_reason_to_exclude(C, V, Y, Expl) :-
    reported(C, V, Y, _, Coverage),
    not(decision(C, V, Y, acceptReported, _, _, _)),
    Prec is Y - 1,
    Succ is Y + 1,
    reported(C, V, Prec, _, CoveragePrec),
    reported(C, V, Succ, _, CoverageSucc),
    sawtooth_threshold(Threshold),
    Coverage - CoveragePrec > Threshold,
    Coverage - CoverageSucc > Threshold,
    concat_atom(
      [ 'Reported data excluded due to an increase from ', CoveragePrec,
        ' percent to ', Coverage, ' percent with decrease to ', % added a word
        CoverageSucc, ' percent. '
      ], Expl).

reported_reason_to_exclude(C, V, Y, Expl) :-
    reported(C, V, Y, _, Coverage),
    not(decision(C, V, Y, acceptReported, _, _, _)),
    Prec is Y - 1,
    Succ is Y + 1,
    reported(C, V, Prec, _, CoveragePrec),
    reported(C, V, Succ, _, CoverageSucc),
    sawtooth_threshold(Threshold),
    CoveragePrec - Coverage > Threshold,
    CoverageSucc - Coverage > Threshold,
    concat_atom(
      [ 'Reported data excluded due to decline in reported coverage from ',
        CoveragePrec, ' percent to ', Coverage,
        ' percent with increase to ', CoverageSucc,' percent. '
      ], Expl).

% Reason to exclude reported: sudden change in most recently reported
% data for classic vaccines.
reported_reason_to_exclude(C, V, Y, Expl) :-
    not(member(V, [pcv3, rotac])),
    reported(C, V, Y, _, Coverage),
    not(decision(C, V, Y, acceptReported, _, _, _)),
    not(reported_later(C, V, Y)),
    Prec is Y - 1,
    reported(C, V, Prec, _, CoveragePrec),
    sawtooth_threshold(Threshold),
    abs(CoveragePrec - Coverage) > Threshold,
    concat_atom(
      [ 'Reported data excluded due to sudden change in coverage from ',
        CoveragePrec, ' level to ', Coverage,' percent. '
      ], Expl).

% Reason to exclude reported: sudden decline in most recently reported
% data for new vaccines.
reported_reason_to_exclude(C, V, Y, Expl) :-
    member(V, [pcv3, rotac]),
    reported(C, V, Y, _, Coverage),
    not(decision(C, V, Y, acceptReported, _, _, _)),
    not(reported_later(C, V, Y)),
    Prec is Y - 1,
    reported(C, V, Prec, _, CoveragePrec),
    sawtooth_threshold(Threshold),
    CoveragePrec - Coverage > Threshold,
    concat_atom(
      [ 'Reported data excluded due to decline in reported coverage from ',
        CoveragePrec,' level to ', Coverage, ' percent. '
      ], Expl).

% Helper predicates
output_results([], _).

output_results([H | T], Out) :-
    output_fields(H, Out),
    output_results(T, Out).

output_fields([H], Out) :-
    write(Out, H),
    nl(Out).

output_fields([H1, H2 | T], Out) :-
    write(Out, H1),
    write(Out, '\t'),
    output_fields([H2 | T], Out).

