import os
import io
from PyPDF2 import PdfFileReader, PdfFileWriter
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter
from PIL import Image

# ...

def add_signature_to_pdf(input_pdf, output_pdf, signature_path, name):
    # Load the pdf
    reader = PdfFileReader(open(input_pdf, "rb"))
    writer = PdfFileWriter()

    # Use PIL to get signature dimensions and resize the image to 20%
    signature_img = Image.open(signature_path)
    sig_width, sig_height = signature_img.size
    sig_width *= 0.2  # Reduce the width to 20%
    sig_height *= 0.2  # Reduce the height to 20%

    # Loop over all pages
    for page_num in range(reader.getNumPages()):
        page = reader.getPage(page_num)

        pdf_width = float(page.mediaBox[2])
        pdf_height = float(page.mediaBox[3])

        # Check if this is the first page (0-indexed)
        if page_num == 0:
            # Compute the position for the signature
            x_position = (pdf_width - sig_width) / 2  # centering the signature
            y_position = pdf_height * 0.4 - sig_height

            # Add the signature
            packet = io.BytesIO()
            can = canvas.Canvas(packet, pagesize=letter)
            can.drawImage(signature_path, x_position, y_position, width=sig_width, height=sig_height)
            can.drawString(x_position, y_position - 20, name)  # 20 units below the signature
            can.save()

            packet.seek(0)
            new_pdf = PdfFileReader(packet)
            page.mergePage(new_pdf.getPage(0))

        writer.addPage(page)

    # Save the output
    with open(output_pdf, 'wb') as f:
        writer.write(f)

# ...

if __name__ == "__main__":
    folder_path = '/Users/xaviergonzalez/Downloads/solicitudes/'  # Update the path to the folder containing the PDFs
    signature_path = '/Users/xaviergonzalez/Library/Mobile Documents/com~apple~CloudDocs/Desktop/9131 2C 2023/Slide1.png'  # Update the path to the signature PNG
    name = "Xavier Ignacio Gonzalez"

    for filename in os.listdir(folder_path):
        if filename.endswith('.pdf'):
            input_path = os.path.join(folder_path, filename)
            output_path = os.path.join(folder_path, filename.replace('.pdf', '_signed.pdf'))
            add_signature_to_pdf(input_path, output_path, signature_path, name)

