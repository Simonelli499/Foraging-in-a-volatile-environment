import scipy.stats as stats
from matplotlib import pyplot as plt
import numpy as np
import seaborn as sns

def boxplot_significance(data, test_type='t-test', combinations=None, bar_space=0.07, y_offset=0): # quindi se non specifico il test_type lui mi farà il t-test

    significant_combinations = []

    # Creo una lista di tutte le possibili coppie di indici da testare (es (1,2), (2,3), (1,3), ecc..)
    # ls = list(range(1, data.shape[1] + 1))
    ls = list(range(0, len(data)))

    if combinations is None:
        combinations = [(ls[x], ls[x + y]) for y in reversed(ls) for x in range((len(ls) - y))]

    # Per ogni coppia, faccio il test di significatività     
    for c in combinations:
        # data1 = data[:,c[0] - 1]
        # data2 = data[:,c[1] - 1]
        data1 = data[c[0]] # la prima condizione che voglio chiamare così sarà sempre 0 - mentre l'ultima 3
        data2 = data[c[1]]
        
        # Faccio il test scelto e calcolo la significatività
        if test_type == 't-test':
            _, p = stats.ttest_ind(data1, data2, alternative='two-sided') 
        elif test_type == 'mann-whitney':     
            _, p = stats.mannwhitneyu(data1, data2, alternative='two-sided') # _ mi permette di ignorare l'output che corrisponderebbe a quello che dovrebbe essere in quella posizione, lo uso quando non mi interessa sapere l'output

        # Salvo solo quelle significative al 5%
        if p < 0.05:
            significant_combinations.append([c, p])


    # Faccio il boxplot and set colors
    plt.boxplot(data)
    ax = plt.gca() #gca: get current axis
    bottom, top = ax.get_ylim()
    yrange = top - bottom

    # Ora voglio mettere le linee con gli asterischi che connettono i box significativi
    for i, significant_combination in enumerate(significant_combinations):

            x1 = significant_combination[0][0] + 1
            x2 = significant_combination[0][1] + 1

            level = len(significant_combinations) - i # per alzare la linea orizzontale sotto agli asterischi rispetto a quel boxplot specifico
            
            # per definirmi le posizioni della parentesi quadra
            # bar_height = (yrange * 0.07 * level) + top
            # bar_tips = bar_height - (yrange * 0.02)
            
            bar_height = (yrange * bar_space * level) + y_offset * yrange
            bar_tips = bar_height - (yrange * 0.02)


            # per plottare la parentesi quadra
            plt.plot(
                [x1, x1, x2, x2],
                [bar_tips, bar_height, bar_height, bar_tips], lw=1, c='k')
            
            p = significant_combination[1]
            if p < 0.001:
                sig_symbol = '***'
            elif p < 0.01:
                sig_symbol = '**'
            elif p < 0.05:
                sig_symbol = '*'
                
            text_height = bar_height + (yrange * 0.01)
            plt.text((x1 + x2) * 0.5, text_height, sig_symbol, ha='center', c='k')

   


def extract_data(data, x, y, per_subject=False):

    # x è una lista di tuple, del tipo x = [ ('LongBatter', True), ('RichEnvironment', False)]
    # y è una stringa, es y = 'BoxOpenedFirstArea'

    # Costruisco la condizione per estrarre i dati
    condition = None

    for i, c in enumerate(x):
        if i == 0:
            condition = data[c[0]] == c[1]
        else:
            condition = condition & (data[c[0]] == c[1])

    if condition is None:
        data_conditioned = data       
    else:
        data_conditioned = data[condition]

    if per_subject:

        subject_ids = data["SubjectCode"].unique()
        subject_number = len(subject_ids)
        mean_per_subject = np.zeros(subject_number)

        for i,id in enumerate(subject_ids):
            data_per_subject = data_conditioned[data_conditioned["SubjectCode"] == id]
            box_opened_per_subject = data_per_subject[y] 
            mean_per_subject[i] = np.mean(box_opened_per_subject.to_numpy())

        return mean_per_subject
    else:
        return data_conditioned[y].to_numpy()


def violinplot_significance(data, test_type='t-test', combinations=None, colors=None, bar_space=0.07, y_offset=0): # quindi se non specifico il test_type lui mi farà il t-test
    significant_combinations = []

    # Creo una lista di tutte le possibili coppie di indici da testare (es (1,2), (2,3), (1,3), ecc..)
    # ls = list(range(1, data.shape[1] + 1))
    ls = list(range(0, len(data)))

    if combinations is None:
        combinations = [(ls[x], ls[x + y]) for y in reversed(ls) for x in range((len(ls) - y))]

    # Per ogni coppia, faccio il test di significatività     
    for c in combinations:
        # data1 = data[:,c[0] - 1]
        # data2 = data[:,c[1] - 1]
        data1 = data[c[0]]
        data2 = data[c[1]]
        
        # Faccio il test scelto e calcolo la significatività
        if test_type == 't-test':
            _, p = stats.ttest_ind(data1, data2, alternative='two-sided') 
        elif test_type == 'mann-whitney':     
            _, p = stats.mannwhitneyu(data1, data2, alternative='two-sided') # _ mi permette di ignorare l'output che corrisponderebbe a quello che dovrebbe essere in quella posizione, lo uso quando non mi interessa sapere l'output

        # Salvo solo quelle significative al 5%
        if p < 0.05:
            significant_combinations.append([c, p])

    # Faccio il boxplot and set colors
    if colors is None:
        sns.violinplot(data)
    else:
        sns.violinplot(data, palette=colors)

    ax = plt.gca() #gca: get current axis
    bottom, top = ax.get_ylim()
    yrange = top - bottom

    # Ora voglio mettere le linee con gli asterischi che connettono i box significativi
    for i, significant_combination in enumerate(significant_combinations):

            x1 = significant_combination[0][0]
            x2 = significant_combination[0][1]

            level = len(significant_combinations) - i # per alzare la linea orizzontale sotto agli asterischi rispetto a quel boxplot specifico
            
            # per definirmi le posizioni della parentesi quadra
            bar_height = (yrange * bar_space * level) + top + y_offset * yrange
            bar_tips = bar_height - (yrange * 0.02)

            # per plottare la parentesi quadra
            plt.plot(
                [x1, x1, x2, x2],
                [bar_tips, bar_height, bar_height, bar_tips], lw=1, c='k')
            
            p = significant_combination[1]
            if p < 0.001:
                sig_symbol = '***'
            elif p < 0.01:
                sig_symbol = '**'
            elif p < 0.05:
                sig_symbol = '*'
                
            text_height = bar_height + (yrange * 0.01)
            plt.text((x1 + x2) * 0.5, text_height, sig_symbol, ha='center', c='k')



def splitedviolinplot_significance(data, test_type='t-test', combinations=None, colors=None): # quindi se non specifico il test_type lui mi farà il t-test
    significant_combinations = []

    # Creo una lista di tutte le possibili coppie di indici da testare (es (1,2), (2,3), (1,3), ecc..)
    # ls = list(range(1, data.shape[1] + 1))
    ls = list(range(0, len(data)))

    if combinations is None:
        combinations = [(ls[x], ls[x + y]) for y in reversed(ls) for x in range((len(ls) - y))]

    # Per ogni coppia, faccio il test di significatività     
    for c in combinations:
        # data1 = data[:,c[0] - 1]
        # data2 = data[:,c[1] - 1]
        data1 = data[c[0]]
        data2 = data[c[1]]
        
        # Faccio il test scelto e calcolo la significatività
        if test_type == 't-test':
            _, p = stats.ttest_ind(data1, data2, alternative='two-sided') 
        elif test_type == 'mann-whitney':     
            _, p = stats.mannwhitneyu(data1, data2, alternative='two-sided') # _ mi permette di ignorare l'output che corrisponderebbe a quello che dovrebbe essere in quella posizione, lo uso quando non mi interessa sapere l'output

        # Salvo solo quelle significative al 5%
        if p < 0.05:
            significant_combinations.append([c, p])

    # Faccio il boxplot and set colors
    if colors is None:
        sns.violinplot(data)
    else:
        sns.violinplot(data,  split=True, inner="quart", gap=-0.1, palette=colors)

    ax = plt.gca() #gca: get current axis
    bottom, top = ax.get_ylim()
    yrange = top - bottom

    # Ora voglio mettere le linee con gli asterischi che connettono i box significativi
    for i, significant_combination in enumerate(significant_combinations):

            x1 = significant_combination[0][0]
            x2 = significant_combination[0][1]

            level = len(significant_combinations) - i # per alzare la linea orizzontale sotto agli asterischi rispetto a quel boxplot specifico
            
            # per definirmi le posizioni della parentesi quadra
            bar_height = (yrange * 0.07 * level) + top
            bar_tips = bar_height - (yrange * 0.02)

            # per definirmi la lunghezza che voglio per la barra della significatività - lo faccio perchè per lo splited violin voglio una barra più piccola
            bar_length = 0.3 # Set the length of the bar to 0.5 units

            # devo calcolare il centro della barra quindi prendo le coordinate x1 e x2 e le divido a metà
            bar_center = (x1 + x2) / 2

            # Calculate the new x coordinates for the bar
            # sottraggo e aggiungo perchè una coordinata è alla sinistra e una alla destra del centro
            # divido per 3 perchè voglio questa bar molto piccina
            x1_smallbar = bar_center - bar_length / 3
            x2_smallbar = bar_center + bar_length / 3

            # per plottare la parentesi quadra
            plt.plot(
                [x1_smallbar, x1_smallbar, x2_smallbar, x2_smallbar],
            [bar_tips, bar_height, bar_height, bar_tips], lw=1, c='k')
      
            p = significant_combination[1]
            if p < 0.001:
                sig_symbol = '***'
            elif p < 0.01:
                sig_symbol = '**'
            elif p < 0.05:
                sig_symbol = '*'
                
            text_height = bar_height + (yrange * 0.01)
            plt.text((x1 + x2) * 0.5, text_height, sig_symbol, ha='center', c='k')

