from scipy.stats import mannwhitneyu, ttest_ind, wilcoxon
from matplotlib import pyplot as plt
from matplotlib.cm import viridis
import numpy as np

from itertools import combinations, chain, product


# def get_combinations_of_factors(factors):
#     # 1. Generate every possible subset of factors (from length 1 to 3)
#     subsets = chain.from_iterable(combinations(factors, r) for r in range(1, len(factors) + 1))

#     # 2. Join subsets with '+' and '*'
#     # We use a set to avoid duplicates like 'A' (which would appear for both + and *)
#     results = set()
#     for s in subsets:
#         results.add(" + ".join(s))
#         if len(s) > 1:
#             results.add(" * ".join(s))

#     # 3. Handle the "Mixed" cases (like 'A*B + C')
#     # This adds the 3-way interaction to the additive main effects
#     if len(factors) > 2:
#         results.add(" + ".join(factors) + " + " + ":".join(factors))

#     combinations_list = sorted(list(results), key=len)
#     return combinations_list


def get_combinations_of_factors(factors):

    all_formulas = []

    # 1. Iterate through all possible subset sizes (1, 2, and 3)
    for r in range(1, len(factors) + 1):
        for sub in combinations(factors, r):
            if len(sub) == 1:
                all_formulas.append(sub[0])
            else:
                # 2. For subsets > 1, generate all operator permutations (+ or *)
                for ops in product([' + ', ' * '], repeat=len(sub)-1):
                    formula = sub[0]
                    for i in range(len(ops)):
                        formula += f"{ops[i]}{sub[i+1]}"
                    all_formulas.append(formula)

    # 3. Add the specific "Additive + 3-way" case
    if len(factors) == 3:
        all_formulas.append(" + ".join(factors) + " + " + ":".join(factors))

    # Clean up: Remove duplicates like "A*B" and "B*A" if order doesn't matter
    return list(dict.fromkeys(all_formulas)) 


def boxplot(data, test_type='wilcoxon', test_combinations=None, multiple_correction=None, custom_significant_combinations=None, show_points=False, show_connecting_lines=False, sep_multiplier=1, connecting_lines_skip=1, box_colors='w', box_widths=0.5, median_colors='k', boxes_alpha=1, linewdith=1, median_linewidth=1, points_colors='gradient', significance_lines_position='up'):

    # Se i dati sono una matrice di numpy, li trasformo in una lista di array
    if type(data) is np.ndarray:
        data = [data[:, i] for i in range(data.shape[1])]

    # Converto per sicurezza tutti gli elementi delle liste in array di numpy
    data = [np.array(d) for d in data]

    # Elimino eventuali nan
    data = [d[np.isnan(d) == False] for d in data]

    # Info sui dati
    boxes_num = len(data)
    samples_num = len(data[0])
    same_num = sum([True if len(data[i]) == len(data[0]) else False for i in range(boxes_num)]) == boxes_num

    # Mostro il boxplot usando la funzione di matplotlib
    bplot = plt.boxplot(data, patch_artist=True, widths=box_widths)

    # Coloro i boxplot e cambio le dimensioni delle linee
    for i, (patch, median) in enumerate(zip(bplot['boxes'], bplot['medians'])):

        if type(box_colors) is str:
            b_col = box_colors
        else:
            b_col = box_colors[i]

        if type(median_colors) is str:
            m_col = median_colors
        else:
            m_col = median_colors[i]

        patch.set_facecolor(b_col)
        median.set_color(m_col)
        patch.set_alpha(boxes_alpha)
        patch.set_linewidth(linewdith)
        median.set_linewidth(median_linewidth)

    # Mostro i punti
    if show_connecting_lines:
        show_points = True

    if show_points:

        if same_num:
            if type(points_colors) == str:
                if points_colors == 'random':
                    points_colors = [[np.random.rand(3) for _ in range(samples_num)]] * boxes_num

                elif points_colors == 'gradient':

                    # Ordino i sample dall'alto verso il basso nel primo box e uso quelli come riferimento
                    if connecting_lines_skip == 1:
                        y = np.array(data[0])
                        y_ind_sort = np.argsort(y)
                        points_colors = np.zeros((samples_num, 4))
                        for i in range(samples_num):
                            points_colors[y_ind_sort[i], :] = viridis(i/samples_num)
                        points_colors = [points_colors] * boxes_num
                    else:
                        points_colors = []
                        for i in range(0, boxes_num):
                            if i % connecting_lines_skip == 0:
                                y = np.array(data[i])
                                y_ind_sort = np.argsort(y)
                            p_col = np.zeros((samples_num, 4))

                            for j in range(samples_num):
                                p_col[y_ind_sort[j], :] = viridis(j/samples_num)
                            points_colors.append(p_col)

                elif points_colors == 'w':

                    points_colors = [np.ones((samples_num, 3))] * boxes_num

            elif type(points_colors) == np.ndarray:

                points_colors = [points_colors] * boxes_num

        else:
            points_colors = [[0, 0, 0]] * boxes_num
            print('Il tipo di colore richiesto per i punti è incompatibile con il fatto che ogni box ha un numero diverso di punti')

        for i in range(boxes_num-1):

            x_values_1 = np.ones(len(data[i])) * i + 1
            x_values_2 = np.ones(len(data[i+1])) * i + 2
            y_values_1 = data[i]
            y_values_2 = data[i+1]

            # Il colore dei punti o è dato da un gradiente (dall'alto in basso)
            # Oppure è dato dall'utente (per ogni box) oppure da un gradiente
            col_1 = points_colors[i]
            col_2 = points_colors[i+1]

            # Punti
            plt.scatter(x_values_1, y_values_1, 10, color=col_1, zorder=10, edgecolors='k')
            plt.scatter(x_values_2, y_values_2, 10, color=col_2, zorder=10, edgecolors='k')

    # Connetto tra loro i punti dei vari boxplot, ma solo per quelli adiacenti
    if show_connecting_lines:

        if same_num:

            for i in range(0, boxes_num-1, connecting_lines_skip):
                x_values_1 = np.ones(samples_num) * i + 1
                x_values_2 = np.ones(samples_num) * i + 2
                y_values_1 = data[i]
                y_values_2 = data[i+1]

                for j in range(samples_num):
                    if y_values_2[j] > y_values_1[j]:
                        line_col = 'g'
                    else:
                        line_col = 'r'

                    plt.plot([x_values_1[j], x_values_2[j]], [y_values_1[j], y_values_2[j]], line_col, alpha=0.2, linewidth=1)
        else:
            print("E' stato richiesto di mostrare le linee che connettono i punti nei box, ma ogni box ha un numero di punti diverso")

    # Creazione combinazioni per il test
    if test_combinations == 'all':
        test_combinations = [(i, j) for i in range(boxes_num) for j in range(i+1, boxes_num)]

    # Faccio i test
    if custom_significant_combinations is None:
        significant_combinations = []

        if test_combinations is not None:
            combinations_num = len(test_combinations)

            for c in test_combinations:

                data_1 = data[c[0]]
                data_2 = data[c[1]]

                if test_type == 't-test':
                    _, p = ttest_ind(data_1, data_2, alternative='two-sided')
                if test_type == 'mann-whitney':
                    _, p = mannwhitneyu(data_1, data_2, alternative='two-sided')
                if test_type == 'wilcoxon':
                    _, p = wilcoxon(data_1, data_2, alternative='two-sided')

                if p <= 0.05:
                    significant_combinations.append((*c, p))

                print(c,p)
        # Correzione di Bonferroni
        if multiple_correction == 'bonferroni':
            for i, c in enumerate(significant_combinations):

                ind_1, ind_2, p = c
                p_mod = p * combinations_num

                if p_mod <= 0.05:
                    significant_combinations[i] = (ind_1, ind_2, p_mod)
                else:
                    significant_combinations.pop(i)
                    i -= 1

        # False discovery rate
        if multiple_correction == 'false-discovery':

            # Ordino la lista di tutti i p-value
            p_values = [c[2] for c in significant_combinations]
            p_sorted_inds = np.argsort(p_values)
            combinations_to_remove = []

            for i in range(len(p_sorted_inds)):
                p_ind = p_sorted_inds[i]
                ind_1, ind_2, p = significant_combinations[p_ind]
                p_mod = p * combinations_num / (i + 1)

                if p_mod <= 0.05:
                    significant_combinations[p_ind] = (ind_1, ind_2, p_mod)
                else:
                    combinations_to_remove.append(significant_combinations[p_ind])

            for c in combinations_to_remove:
                significant_combinations.remove(c)
    else:
        significant_combinations = custom_significant_combinations

    # Plotto le differenze significative
    if len(significant_combinations) > 0:

        print(significant_combinations)
        # Altezza iniziale per le linee
        ylim = plt.ylim()
        y_shift = 0.03 * (ylim[1] - ylim[0]) * sep_multiplier

        # Riordino le combinazioni in funzione della distanza tra i boxplot che le compongono
        box_dist = [abs(c[0] - c[1]) for c in significant_combinations]
        box_order = np.argsort(box_dist)
        significant_combinations_ordered = [significant_combinations[box_order[i]] for i in range(len(significant_combinations))]

        line_heights_up = np.array([np.max(d) for d in data])
        line_heights_down = np.array([np.min(d) for d in data])

        for i, c in enumerate(significant_combinations_ordered):
            ind1, ind2, p_value = c

            if p_value < 0.001:
                asterisks = '***'
            elif p_value < 0.01:
                asterisks = '**'
            elif p_value < 0.05:
                asterisks = '*'

            # Vedo l'altezza massima registrata di tutti i boxplot tra questi due
            # compresi essi stessi
            if significance_lines_position == 'up':
                line_pos = 'up'
            elif significance_lines_position == 'down':
                line_pos = 'down'
            elif type(significance_lines_position) is dict:
                # Cerco quella corrispondente
                if (ind1, ind2) in significance_lines_position:
                    line_pos = significance_lines_position[(ind1, ind2)]
                else:
                    line_pos = 'up'
            else:
                print('Tipo di posizionamento linee significatività non supportato')

            if line_pos == 'up':
                height = np.max(line_heights_up[ind1:(ind2+1)]) + y_shift
                tips_height = height
                line_height = tips_height + y_shift / 2
                line_heights_up[ind1:(ind2+1)] = height + y_shift * 2

            if line_pos == 'down':
                height = np.min(line_heights_down[ind1:(ind2+1)]) - y_shift
                tips_height = height
                line_height = tips_height - y_shift / 2
                line_heights_down[ind1:(ind2+1)] = height - y_shift * 2

            # Draw the significance line
            plt.plot([ind1 + 1, ind1 + 1, ind2 + 1, ind2 + 1], [tips_height, line_height, line_height, tips_height], lw=1, c='k')

            # Draw the asterisk for significance
            if line_pos == 'up':
                t_asterisk = plt.text((ind1 + ind2 + 2) * .5, line_height + y_shift * 0.3, asterisks, ha='center', va='bottom', color=[0.2, 0.2, 0.2])
            if line_pos == 'down':
                t_asterisk = plt.text((ind1 + ind2 + 2) * .5, line_height - y_shift * 0.3, asterisks, ha='center', va='top', color=[0.2, 0.2, 0.2])

            t_asterisk.set_bbox(dict(facecolor='white', alpha=1, linewidth=0, pad=0.05))

