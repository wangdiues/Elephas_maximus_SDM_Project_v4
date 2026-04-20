
from PIL import Image, ImageDraw, ImageFont
import os, textwrap

RUN_DIR  = r'E:/Elephas_maximus_SDM_Project_v4/04_outputs/runs/RUN_20260326_202009_b990/08_figures_tables'
OUT_PATH = r'E:/Elephas_maximus_SDM_Project_v4/04_outputs/facebook_post.png'

FIGURES = [
    'figure_01_present_suitability.png',
    'figure_future_near_2050.png',
    'figure_E05_pa_overlay.png',
    'figure_conflict_present.png',
    'figure_E07_variable_importance.png',
    'figure_gcm_trajectories.png',
    'figure_uncertainty_2021_2050.png',
    'figure_s17_dzongkhag_suitability.png',
    'figure_02_model_auc.png',
]

BG     = (15, 40, 30)
ACCENT = (72, 175, 108)
WHITE  = (255, 255, 255)
SOFT   = (200, 225, 210)
GOLD   = (235, 185, 50)

def font(size, bold=False):
    names = (['arialbd.ttf'] if bold else []) + ['arial.ttf']
    for n in names:
        try:
            return ImageFont.truetype(f'C:/Windows/Fonts/{n}', size)
        except OSError:
            pass
    return ImageFont.load_default()

F_TITLE = font(40, bold=True)
F_SUB   = font(21)
F_STAT  = font(30, bold=True)
F_BODY  = font(18)
F_TINY  = font(14)
F_TAG   = font(15)

W        = 1200
HEADER_H = 155
FOOTER_H = 250
PAD      = 10
COLS     = 3
ROWS     = 3
CELL_W   = (W - PAD * (COLS + 1)) // COLS
CELL_H   = CELL_W
GRID_H   = ROWS * CELL_H + (ROWS + 1) * PAD
H        = HEADER_H + GRID_H + FOOTER_H

canvas = Image.new('RGB', (W, H), BG)
draw   = ImageDraw.Draw(canvas)

draw.rectangle([(0,0),(W,5)], fill=ACCENT)
draw.text((W//2, 45), 'Asian Elephant Habitat Suitability in Bhutan', font=F_TITLE, fill=WHITE, anchor='mm')
draw.text((W//2, 88), 'Species Distribution Modelling  |  Present & Future Climate (CMIP6)', font=F_SUB, fill=SOFT, anchor='mm')
draw.text((W//2, 118), 'GLM  .  Random Forest  .  GBM  .  MaxEnt  .  AUC-weighted Ensemble', font=F_TINY, fill=ACCENT, anchor='mm')
draw.text((W//2, 140), 'Elephas maximus  --  Bhutan National Extent', font=F_TINY, fill=SOFT, anchor='mm')
draw.rectangle([(0, HEADER_H-4),(W, HEADER_H)], fill=ACCENT)

for idx, fname in enumerate(FIGURES[:COLS*ROWS]):
    col = idx % COLS
    row = idx // COLS
    x0  = PAD + col * (CELL_W + PAD)
    y0  = HEADER_H + PAD + row * (CELL_H + PAD)
    x1  = x0 + CELL_W
    y1  = y0 + CELL_H
    path = os.path.join(RUN_DIR, fname)
    if os.path.exists(path):
        img = Image.open(path).convert('RGB')
        iw, ih = img.size
        scale  = max(CELL_W / iw, CELL_H / ih)
        nw, nh = int(iw * scale), int(ih * scale)
        img    = img.resize((nw, nh), Image.LANCZOS)
        cx     = (nw - CELL_W) // 2
        cy     = (nh - CELL_H) // 2
        img    = img.crop((cx, cy, cx + CELL_W, cy + CELL_H))
        canvas.paste(img, (x0, y0))
        draw.rectangle([(x0,y0),(x1,y1)], outline=ACCENT, width=2)
    else:
        draw.rectangle([(x0,y0),(x1,y1)], fill=(30,55,42))

fy0 = HEADER_H + GRID_H
draw.rectangle([(0, fy0),(W, fy0+4)], fill=ACCENT)
draw.rectangle([(0, fy0+4),(W, H)], fill=(20,50,36))

STATS = [
    ('12.5%',  'Core Climate Refugia'),
    ('46.7%',  'Refugia Inside PAs'),
    ('96',     'Future Scenarios'),
    ('252',    'Presence Records'),
]
box_w  = (W - 60) // 4
box_h  = 95
box_y0 = fy0 + 18
for i, (num, lbl) in enumerate(STATS):
    bx0 = 15 + i * (box_w + 12)
    bx1 = bx0 + box_w
    draw.rounded_rectangle([(bx0,box_y0),(bx1,box_y0+box_h)], radius=8, fill=(28,68,50), outline=ACCENT, width=2)
    draw.text(((bx0+bx1)//2, box_y0+30), num, font=F_STAT, fill=GOLD, anchor='mm')
    draw.text(((bx0+bx1)//2, box_y0+62), lbl, font=F_TINY, fill=SOFT, anchor='mm')

body = ('Our ensemble model maps where Asian elephants can persist in Bhutan today and across 96 future climate scenarios. '
        'Only 12.5%% of Bhutan qualifies as climatically stable core refugia -- and over half lies outside protected areas, '
        'revealing critical conservation gaps for this endangered species.')
ty = box_y0 + box_h + 20
for line in textwrap.wrap(body, width=100):
    draw.text((W//2, ty), line, font=F_BODY, fill=SOFT, anchor='mm')
    ty += 26

tags = '#AsianElephant  #Bhutan  #ClimateChange  #ConservationScience  #SDM  #WildlifeConservation  #OpenScience'
draw.text((W//2, H-22), tags, font=F_TAG, fill=ACCENT, anchor='mm')

canvas.save(OUT_PATH, 'PNG', dpi=(150,150))
print('DONE', canvas.size)
