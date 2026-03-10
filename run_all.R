# ==============================================================================
# QUY TRÌNH TỰ ĐỘNG HÓA PHÂN TÍCH RỦI RO DE-PEG STABLECOIN
# Script: run_all.R
# Purpose: Điều phối và chạy toàn bộ Pipeline từ thu thập đến mô hình hóa
# ==============================================================================

# --- [1] KIỂM TRA & ĐỒNG BỘ MÔI TRƯỜNG ---
message(">>> [BƯỚC 0] Kiểm tra thư viện hệ thống...")
if (!require("renv")) install.packages("renv")
# Tự động cài đặt các gói thiếu (như skimr, fixest...) dựa trên renv.lock
renv::restore(prompt = FALSE)

# --- [2] ĐỊNH NGHĨA DANH SÁCH CÁC FILE CHẠY ---
# Thứ tự được sắp xếp theo luồng dữ liệu (Dependency-aware)
pipeline_scripts <- c(
    "code/crawl_fear_and_greed.R", # Lấy dữ liệu tâm lý
    "code/crawl_market_cap.R", # Lấy dữ liệu vốn hóa
    "code/crawl_top5.R", # Lấy dữ liệu giá BTC/ETH
    "code/collect_data.R", # Hợp nhất dữ liệu Stablecoin
    "code/panel_2.R", # Feature Engineering & Lagging
    "code/eda.R", # Thống kê mô tả & EDA
    "code/simple_model.R" # Hồi quy Logit & Xuất kết quả
)

# --- [3] HÀM THỰC THI PIPELINE ---
run_pipeline <- function(scripts) {
    start_total <- Sys.time()
    cat("\n", rep("=", 60), "\n", sep = "")
    cat("   BẮT ĐẦU CHẠY HỆ THỐNG PHÂN TÍCH\n")
    cat(rep("=", 60), "\n", sep = "")

    success_log <- data.frame(Script = scripts, Status = "Pending", Time = NA)

    for (i in seq_along(scripts)) {
        script_path <- scripts[i]

        if (!file.exists(script_path)) {
            message(paste("\n [LỖI] Không tìm thấy file:", script_path))
            success_log$Status[i] <- "Missing"
            break
        }

        message(paste("\n>>> [", i, "/", length(scripts), "] Đang thực thi:", script_path))
        start_script <- Sys.time()

        # Chạy script trong một môi trường mới để tránh xung đột biến
        try_run <- try(source(script_path, local = FALSE))

        end_script <- Sys.time()
        duration <- round(as.numeric(difftime(end_script, start_script, units = "secs")), 2)

        if (inherits(try_run, "try-error")) {
            message(paste(" [!] Lỗi tại:", script_path))
            success_log$Status[i] <- "Failed"
            break # Dừng pipeline nếu một bước bị lỗi để bảo vệ dữ liệu
        } else {
            success_log$Status[i] <- "Success"
            success_log$Time[i] <- paste0(duration, "s")
            message(paste(" OK - Hoàn thành trong", duration, "giây."))
        }
    }

    end_total <- Sys.time()
    total_duration <- round(as.numeric(difftime(end_total, start_total, units = "mins")), 2)

    # --- [4] TỔNG KẾT ---
    cat("\n", rep("=", 60), "\n", sep = "")
    cat("                TỔNG KẾT QUY TRÌNH CHẠY\n")
    cat(rep("-", 60), "\n", sep = "")
    print(success_log)
    cat(rep("-", 60), "\n", sep = "")
    cat("Tổng thời gian thực hiện:", total_duration, "phút.\n")

    if (all(success_log$Status == "Success")) {
        cat(" KẾT QUẢ: Toàn bộ biểu đồ và báo cáo đã sẵn sàng tại /outputs/\n")
    } else {
        cat(" KẾT QUẢ: Quy trình bị gián đoạn. Vui lòng kiểm tra file lỗi.\n")
    }
    cat(rep("=", 60), "\n", sep = "")
}

# --- [5] KÍCH HOẠT ---
run_pipeline(pipeline_scripts)
