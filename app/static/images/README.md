# Static Images Directory

This directory contains static assets for the WBES Dashboard.

## Current Files

- `logo.svg` - Placeholder logo (SVG format)
- `favicon.svg` - Placeholder favicon (SVG format)

## Customization

To customize the dashboard branding:

1. **Logo** (referenced in `app/main.R` line 49):
   - Replace `logo.svg` with `logo.png` (recommended: 200x40 pixels)
   - Or keep SVG format for scalability
   - Update the file reference in `app/main.R` if changing the filename

2. **Favicon** (referenced in `app/main.R` line 62):
   - Replace `favicon.svg` with `favicon.png` or `favicon.ico`
   - Recommended size: 32x32 pixels or 16x16 pixels
   - Update the file reference in `app/main.R` if changing format

## Branding Colors

The dashboard uses the following Kwiz Research theme colors:
- Primary (Teal): #1B6B5F
- Secondary (Coral): #F49B7A
- Background: #FFFFFF
- Text: #333333

Consider using these colors in your branding for consistency.
